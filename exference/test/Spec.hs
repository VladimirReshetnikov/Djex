{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (bracket)
import Data.Monoid (Any (..))
import Data.Bifunctor (first)
import Data.Data
  ( dataTypeConstrs
  , dataTypeName
  , dataTypeOf
  , fromConstr
  , gmapQ
  , showConstr
  , toConstr
  )
import Data.Either (rights)
import Data.Functor.Identity (runIdentity)
import Data.List (find, isInfixOf, isPrefixOf)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC.Generics as Generic
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, hPutStr, openTempFile)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.MultiRWS (runMultiRWSTNil, withMultiWriterAW)
import qualified Language.Haskell.Exts.Syntax as HSE
import qualified Language.Haskell.Exts.Parser as HSE
import qualified Language.Haskell.Exts.Pretty as HSE
import qualified Language.Haskell.Exts.Extension as HSE
import qualified Language.Haskell.Exts.SrcLoc as HSE

import Language.Haskell.Exference.Core
  ( ExferenceChunkElement (..)
  , ExferenceHeuristicsConfig (..)
  , SearchCompletion (..)
  , SearchStatus (..)
  , SearchStatusError (..)
  , constraintsRelaxedAtStep
  , findExpressionsWithStats
  , findExpressionsWithStatsEither
  , toSearchProgress
  , toSearchBatch
  , toGeneratedSearchBatch
  , validateExferenceInput
  )
import Language.Haskell.Exference.Core.ConstraintSolver
import Language.Haskell.Exference.Core.Declaration
import Language.Haskell.Exference.Core.Expression
  ( Expression (..)
  , ExpressionRenderError (..)
  , renderExpression
  , showExpression
  , toGeneratedExpression
  )
import Language.Haskell.Exference.Core.ExpressionCheck
import Language.Haskell.Exference.Core.ExpressionSimplify (simplifyExpression)
import Language.Haskell.Exference.Core.FunctionBinding
  ( ConstructorBinding (..)
  , DeconstructorBinding (..)
  , EnvDictionary (..)
  , FunctionBinding (..)
  )
import qualified Language.Haskell.Exference.Core.Internal.Scope as Scope
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.Unify
import Language.Haskell.Exference
  ( ExferenceInput (..)
  , ExferenceInputError (..)
  , ExferenceStats (..)
  , Penalty (..)
  , SearchSelection (..)
  , findExpressionsEither
  , findOneExpression
  , selectBestNExpressions
  , selectFirstBestExpressions
  , selectFirstBestExpressionsLookahead
  , selectFirstBestExpressionsLookaheadPreferNoConstraints
  , selectFirstExpressionLookahead
  , selectOneExpression
  , selectSortNExpressions
  )
import Language.Haskell.Exference.EnvironmentParser
  ( EnvironmentLoadError (..)
  , SourceEnvironment (..)
  , checkedSourceInventory
  , checkedSourceProjection
  , compileWithDict
  , environmentFromModuleAndRatings
  , environmentFromPath
  , parseModules
  , parseRatings
  , toSynthesisSourceEnvironment
  , toSynthesisSourceInventory
  )
import Language.Haskell.Exference.ClassEnvFromHaskellSrc
  (ClassEnvironmentLoadError (..), getClassEnv)
import Language.Haskell.Exference.Diagnostic
import Language.Haskell.Exference.ExpressionToHaskellSrc (convert, convertToFunc)
import Language.Haskell.Exference.BindingsFromHaskellSrc
  (getClassMethods, getDataConss)
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
  ( HsTypeDecl (..)
  , applyTypeDecls
  , fromSynthesisTypeDeclaration
  , getTypeDecls
  , parseType
  , toSynthesisTypeDeclaration
  )
import Language.Haskell.Exference.TypeFromHaskellSrc
  ( haskellSrcExtsParseMode
  , parseQualifiedName
  , convertQName
  )
import Language.Haskell.Exference.SimpleDict (defaultHeuristicsConfig, emptyClassEnv)
import qualified Language.Haskell.Synthesis.Constraint as SharedConstraint
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Generated as Generated
import qualified Language.Haskell.Synthesis.Declaration as SharedDeclaration
import qualified Language.Haskell.Synthesis.Environment as SharedEnvironment
import qualified Language.Haskell.Synthesis.Kind as SharedKind
import qualified Language.Haskell.Synthesis.KindInference as SharedKindInference
import qualified Language.Haskell.Synthesis.Inventory as SharedInventory
import qualified Language.Haskell.Synthesis.Search as SharedSearch
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified CompatibilityImport
import Paths_exference (getDataFileName)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Exference"
  [ testGroup "class environments"
      [ testCase "class declarations contain only finite name references" $ do
          let self = HsTypeClass (name "Self") [0]
                [HsConstraint (name "Self") [TypeVar 0]]
              classA = HsTypeClass (name "A") [0]
                [HsConstraint (name "B") [TypeVar 0]]
              classB = HsTypeClass (name "B") [0]
                [HsConstraint (name "A") [TypeVar 0]]
              declarations = [self, classA, classB]
          force declarations @?= declarations
          assertBool "showing recursive class declarations did not terminate"
            $ not $ null $ force $ show declarations
      , testCase "superclass closure follows declarations by name" $ do
          let classA = HsTypeClass (name "A") [0] []
              classB = HsTypeClass (name "B") [0]
                [HsConstraint (name "A") [TypeVar 0]]
              constraints = Set.singleton
                $ HsConstraint (name "B") [TypeVar 1]
          environment <- expectRight $ mkStaticClassEnv [classA, classB] []
          Set.map constraint_tclass
            (inflateHsConstraints environment constraints)
            @?= Set.fromList [name "A", name "B"]
      , testCase "superclass cycles are rejected explicitly" $ do
          let classA = HsTypeClass (name "A") [0]
                [HsConstraint (name "B") [TypeVar 0]]
              classB = HsTypeClass (name "B") [0]
                [HsConstraint (name "A") [TypeVar 0]]
          case mkStaticClassEnv [classA, classB] [] of
            Left (SuperclassCycle cycleNames) ->
              Set.fromList cycleNames @?= Set.fromList [name "A", name "B"]
            result -> fail $ "superclass cycle was not diagnosed: " ++ show result
      , testCase "self-superclass cycles are rejected explicitly" $ do
          let self = HsTypeClass (name "Self") [0]
                [HsConstraint (name "Self") [TypeVar 0]]
          mkStaticClassEnv [self] []
            @?= Left (SuperclassCycle [name "Self"])
      , testCase "shared constraint conversion is lossless" $ do
          let constraint = HsConstraint (name "C")
                [TypeApp (TypeCons $ name "Maybe") (TypeVar 0)]
          fromSynthesisConstraint (toSynthesisConstraint constraint)
            @?= Right constraint
      , testCase "shared constraints reject unboxed class names" $ do
          unboxed <- expectRight $ SharedName.tupleName SharedName.Unboxed 2
          case fromSynthesisConstraint
              (SharedConstraint.Constraint unboxed [TypeVar 0]) of
            Left (UnsupportedSpecialName _) -> pure ()
            result -> fail $ "unboxed constraint was accepted: " ++ show result
      , testCase "adding constraints retains existing constraints" $ do
          let cls = HsTypeClass (name "C") [0] []
          staticEnvironment <- expectRight $ mkStaticClassEnv [cls] []
          let env0 = mkQueryClassEnv staticEnvironment
                [HsConstraint (name "C") [TypeVar 1]]
              env1 = addQueryClassEnv
                [HsConstraint (name "C") [TypeVar 2]] env0
          qClassEnv_constraints env1 @?= Set.fromList
            [ HsConstraint (name "C") [TypeVar 1]
            , HsConstraint (name "C") [TypeVar 2]
            ]
      , testCase "duplicate class names are rejected in either order" $ do
          let unary = HsTypeClass (name "C") [0] []
              binary = HsTypeClass (name "C") [0, 1] []
              expected = Left (DuplicateClassDeclaration $ name "C")
          mkStaticClassEnv [unary, binary] [] @?= expected
          mkStaticClassEnv [binary, unary] [] @?= expected
      , testCase "duplicate instance heads are rejected before inflation" $ do
          let cls = HsTypeClass (name "C") [0] []
              headConstraint = HsConstraint (name "C")
                [TypeCons $ name "Int"]
              firstInstance = HsInstance [] headConstraint
              secondInstance = HsInstance
                [HsConstraint (name "C") [TypeVar 0]] headConstraint
          mkStaticClassEnv [cls] [firstInstance, secondInstance] @?=
            Left (DuplicateInstanceHeads [headConstraint])
      , testCase "class declarations require the constructor namespace" $ do
          let lowercase = HsTypeClass (name "className") [] []
              tuple = HsTypeClass (TupleCon 2) [] []
          mkStaticClassEnv [lowercase] []
            @?= Left (InvalidClassName $ name "className")
          mkStaticClassEnv [tuple] []
            @?= Left (InvalidClassName $ TupleCon 2)
      , testCase "duplicate class parameters are rejected" $ do
          let malformed = HsTypeClass (name "C") [0, 0] []
          mkStaticClassEnv [malformed] []
            @?= Left (DuplicateClassParameter (name "C") 0)
      , testCase "negative class parameters are rejected" $ do
          let malformed = HsTypeClass (name "C") [-1] []
          mkStaticClassEnv [malformed] []
            @?= Left (NegativeClassParameter (name "C") (-1))
          assertBool "negative variable ID was mistaken for the ground sentinel"
            $ constraintContainsVariables
            $ HsConstraint (name "C") [TypeVar (-1)]
      , testCase "superclasses cannot mention undeclared parameters" $ do
          let base = HsTypeClass (name "Base") [0] []
              malformed = HsTypeClass (name "Derived") [0]
                [HsConstraint (name "Base") [TypeVar 1]]
          mkStaticClassEnv [base, malformed] []
            @?= Left (UndeclaredSuperclassVariables
              (name "Derived") [1])
      , testCase "unknown superclasses are rejected nominally" $ do
          let malformed = HsTypeClass (name "Derived") [0]
                [HsConstraint (name "Missing") [TypeVar 0]]
          mkStaticClassEnv [malformed] []
            @?= Left (UnknownConstraintClass
              (ClassSuperclass $ name "Derived") (name "Missing"))
      , testCase "qualified classes with the same occurrence remain distinct" $ do
          classAName <- expectRight $ mkQualifiedName ["A"] "C"
          classBName <- expectRight $ mkQualifiedName ["B"] "C"
          let classA = HsTypeClass classAName [0] []
              classB = HsTypeClass classBName [0, 1] []
          environment <- expectRight $ mkStaticClassEnv [classB, classA] []
          Map.keys (sClassEnv_tclasses environment)
            @?= [classAName, classBName]
      , testCase "frontend keeps equally spelled qualified classes distinct" $ do
          environment <- classEnvironmentFromSources
            [ unlines
                [ "module A where"
                , "class C a where"
                ]
            , unlines
                [ "module B where"
                , "class C a b where"
                ]
            ]
            >>= expectRight
          classAName <- expectRight $ mkQualifiedName ["A"] "C"
          classBName <- expectRight $ mkQualifiedName ["B"] "C"
          Map.keys (sClassEnv_tclasses environment)
            @?= [classAName, classBName]
      , testCase "frontend rejects duplicate classes independently of order" $ do
          let source firstDeclaration secondDeclaration = unlines
                [ "module M where"
                , firstDeclaration
                , secondDeclaration
                ]
              unary = "class C a where"
              binary = "class C a b where"
              expected = Left $ ClassDeclarationErrors
                ("duplicate type class: C (M.C)" :| [])
          forwardResult <- classEnvironmentFromSources
            [source unary binary]
          reverseResult <- classEnvironmentFromSources
            [source binary unary]
          forwardResult @?= expected
          reverseResult @?= expected
      , testCase "superclass arity is checked against the class table" $ do
          let binary = HsTypeClass (name "Binary") [0, 1] []
              derived = HsTypeClass (name "Derived") [0]
                [HsConstraint (name "Binary") [TypeVar 0]]
          mkStaticClassEnv [binary, derived] []
            @?= Left (ConstraintArityMismatch
              (ClassSuperclass $ name "Derived") (name "Binary") 2 1)
      , testCase "frontend diagnoses too few and too many superclass arguments" $ do
          let expectedTooFew =
                "wrong number of parameters for type class C: expected 2, got 1"
              expectedTooMany =
                "wrong number of parameters for type class C: expected 2, got 3"
          result <- classEnvironmentFromSources
            [ unlines
                [ "module M where"
                , "class C a b where"
                , "class C a => TooFew a where"
                , "class C a b a => TooMany a where"
                ]
            ]
          case result of
            Left (ClassDeclarationErrors (firstError :| remaining)) -> do
              let errors = firstError : remaining
              assertBool ("missing too-few diagnostic: " ++ show errors)
                $ expectedTooFew `elem` errors
              assertBool ("missing too-many diagnostic: " ++ show errors)
                $ expectedTooMany `elem` errors
            other -> fail $ "malformed superclasses were accepted: " ++ show other
      , testCase "frontend binds head variables before superclass arguments" $ do
          environment <- classEnvironmentFromSources
            [ unlines
                [ "module M where"
                , "class Pair a b where"
                , "class Pair b a => Swap a b where"
                ]
            ]
            >>= expectRight
          pairName <- expectRight $ mkQualifiedName ["M"] "Pair"
          swapName <- expectRight $ mkQualifiedName ["M"] "Swap"
          case Map.lookup swapName (sClassEnv_tclasses environment) of
            Nothing -> fail "Swap class was not elaborated"
            Just declaration -> do
              tclass_params declaration @?= [0, 1]
              tclass_constraints declaration @?=
                [HsConstraint pairName [TypeVar 1, TypeVar 0]]
      , testCase "explicit instance foralls bind every used variable" $ do
          result <- classEnvironmentFromSources
            [ unlines
                [ "module M where"
                , "class C a where"
                , "instance forall a. C b"
                ]
            ]
          case result of
            Left (InstanceDeclarationErrors (firstError :| remaining)) ->
              let errors = firstError : remaining
              in assertBool
                  ("missing explicit-forall diagnostic: " ++ show errors)
                  $ any ("outside its explicit forall" `isInfixOf`) errors
            other -> fail $ "malformed instance was accepted: " ++ show other
      , testCase "instance-head arity is checked against the class table" $ do
          let cls = HsTypeClass (name "C") [0, 1] []
              tooMany = HsInstance [] $ HsConstraint (name "C")
                [ TypeCons $ name "Int"
                , TypeCons $ name "Bool"
                , TypeCons $ name "Char"
                ]
          mkStaticClassEnv [cls] [tooMany]
            @?= Left (ConstraintArityMismatch
              InstanceHead (name "C") 2 3)
      , testCase "unknown instance heads preserve the head site" $ do
          let instanceDeclaration = HsInstance []
                $ HsConstraint (name "Missing") []
          mkStaticClassEnv [] [instanceDeclaration]
            @?= Left (UnknownConstraintClass InstanceHead (name "Missing"))
      , testCase "instance-prerequisite arity is checked" $ do
          let prerequisiteClass = HsTypeClass (name "C") [0, 1] []
              headClass = HsTypeClass (name "D") [] []
              instanceDeclaration = HsInstance
                [HsConstraint (name "C") [TypeCons $ name "Int"]]
                (HsConstraint (name "D") [])
          mkStaticClassEnv [prerequisiteClass, headClass]
              [instanceDeclaration]
            @?= Left (ConstraintArityMismatch
              (InstancePrerequisite $ name "D") (name "C") 2 1)
      , testCase "unknown instance prerequisites preserve the head site" $ do
          let headClass = HsTypeClass (name "D") [] []
              instanceDeclaration = HsInstance
                [HsConstraint (name "Missing") []]
                (HsConstraint (name "D") [])
          mkStaticClassEnv [headClass] [instanceDeclaration]
            @?= Left (UnknownConstraintClass
              (InstancePrerequisite $ name "D") (name "Missing"))
      , testCase "instance inflation substitutes nominal superclass heads" $ do
          let base = HsTypeClass (name "Base") [0] []
              derived = HsTypeClass (name "Derived") [7]
                [HsConstraint (name "Base") [TypeVar 7]]
              integer = TypeCons $ name "Int"
              sourceInstance = HsInstance []
                $ HsConstraint (name "Derived") [integer]
              impliedInstance = HsInstance []
                $ HsConstraint (name "Base") [integer]
          environment <- expectRight
            $ mkStaticClassEnv [base, derived] [sourceInstance]
          sClassEnv_explicitInstances environment @?= [sourceInstance]
          Map.lookup (name "Base") (sClassEnv_instances environment)
            @?= Just [impliedInstance]
      , testCase "class methods attach to the exactly qualified class" $ do
          classAName <- expectRight $ mkQualifiedName ["A"] "C"
          classBName <- expectRight $ mkQualifiedName ["B"] "C"
          let classA = HsTypeClass classAName [0] []
              classB = HsTypeClass classBName [0] []
          parsedModule <- expectParsedModule $ unlines
            [ "module A where"
            , "class C a where"
            , "  method :: a -> a"
            ]
          let methods = runIdentity $ runMultiRWSTNil
                $ getClassMethods
                    (Map.fromList [(classBName, classB), (classAName, classA)])
                    [] Map.empty [parsedModule]
          case rights methods of
            [(_, TypeForall _ (constraint : _) _)] ->
              constraint_tclass constraint @?= classAName
            result -> fail $ "unexpected class methods: " ++ show result
      , testCase "cyclic instance prerequisites remain unresolved" $ do
          let cls = HsTypeClass (name "C") [0] []
              prerequisite = HsConstraint (name "C") [TypeVar 0]
              query = HsConstraint (name "C") [TypeCons $ name "Int"]
              cyclicInstance = HsInstance [prerequisite] prerequisite
          staticEnvironment <- expectRight
            $ mkStaticClassEnv [cls] [cyclicInstance]
          let environment = mkQueryClassEnv staticEnvironment []
          isPossible environment [query] @?= Just [query]
          filterUnresolved environment [query] @?= Just [query]
      ]
  , testGroup "Haskell source bindings"
      [ testCase "monomorphic deconstructors have no empty forall wrapper" $ do
          parsedModule <- expectParsedModule $ unlines
            [ "module Fixture where"
            , "data Flag = Off | On"
            ]
          flag <- expectRight $ mkQualifiedName ["Fixture"] "Flag"
          let extracted = runIdentity $ runMultiRWSTNil
                $ getDataConss Map.empty [] Map.empty [parsedModule]
          case extracted of
            [Right (_, DeconstructorBinding input _ _)] ->
              input @?= TypeCons flag
            result -> fail $ "unexpected datatype bindings: " ++ show result
      , testCase "constructor forall scopes over fields and polymorphic result" $ do
          parsedModule <- expectParsedModule $ unlines
            [ "module Fixture where"
            , "data Choice a = This a | That"
            ]
          choice <- expectRight $ mkQualifiedName ["Fixture"] "Choice"
          this <- expectRight $ mkQualifiedName ["Fixture"] "This"
          that <- expectRight $ mkQualifiedName ["Fixture"] "That"
          let resultType = TypeApp (TypeCons choice) (TypeVar 0)
              extracted = runIdentity $ runMultiRWSTNil
                $ getDataConss Map.empty [] Map.empty [parsedModule]
          case extracted of
            [Right (constructors, DeconstructorBinding input fields False)] -> do
              input @?= resultType
              constructors @?=
                [ (this, TypeForall [0] []
                    $ TypeArrow (TypeVar 0) resultType)
                , (that, TypeForall [0] [] resultType)
                ]
              fields @?=
                [ ConstructorBinding this [TypeVar 0]
                , ConstructorBinding that []
                ]
            result -> fail $ "unexpected datatype bindings: " ++ show result
      ]
  , testGroup "lexical scopes"
      [ testCase "safe scopes expose innermost bindings before ancestors" $ do
          let root = Scope.initialScopeId
          scopes1 <- expectRight
            $ Scope.scopesAddBinding root "root" Scope.initialScopes
          (child, scopes2) <- expectRight $ Scope.addScope root scopes1
          scopes3 <- expectRight
            $ Scope.scopesAddBinding child "child" scopes2
          (leaf, scopes4) <- expectRight $ Scope.addScope child scopes3
          scopes5 <- expectRight
            $ Scope.scopesAddBinding leaf "leaf" scopes4
          Scope.scopeGetAllBindings leaf scopes5
            @?= Right ["leaf", "child", "root"]
      , testCase "checked lookup diagnoses a stale scope ID" $ do
          let oldScopes = Scope.initialScopes :: Scope.Scopes ()
          (child, _newScopes) <- expectRight
            $ Scope.addScope Scope.initialScopeId oldScopes
          Scope.scopeGetAllBindings child oldScopes
            @?= Left (Scope.MissingScopeId 1 [1])
      , testCase "raw scope validation diagnoses a dangling parent" $
          Scope.validateScopeParentGraph
            (IntMap.fromList [(0, Just 99), (1, Nothing)])
            @?= Left (Scope.MissingScopeId 99 [0, 99])
      , testCase "raw scope validation diagnoses the exact cycle" $
          Scope.validateScopeParentGraph
            (IntMap.fromList
              [(0, Just 1), (1, Just 2), (2, Just 1)])
            @?= Left (Scope.ScopeParentCycle [1, 2, 1])
      ]
  , testGroup "type traversal"
      [ testCase "forall substitution protects context binders" $ do
          let ty = TypeForall [0]
                [HsConstraint (name "C") [TypeVar 0, TypeVar 1]]
                (TypeVar 0)
              replacement = TypeCons (name "Replacement")
              (changed, actual) = applySubsts
                (IntMap.fromList [(0, replacement), (1, replacement)]) ty
          getAny changed @?= True
          actual @?= TypeForall [0]
            [HsConstraint (name "C") [TypeVar 0, replacement]]
            (TypeVar 0)
      , testCase "free variables include a forall context" $ do
          let ty = TypeForall [0]
                [HsConstraint (name "C") [TypeVar 0, TypeVar 1]]
                (TypeVar 0)
          freeVars ty @?= Set.singleton 1
      , testCase "largestId includes forall binders and context variables" $ do
          let ty = TypeForall [7]
                [HsConstraint (name "C") [TypeVar 9]] (TypeVar 2)
          largestId ty @?= 9
      , testCase "quantified type rendering binds its body spelling" $ do
          let names = Map.fromList [("x", 0)]
          showHsType names (TypeForall [0] [] $ TypeVar 0)
            @?= "forall x . x"
          showHsType names
            (TypeForall []
              [HsConstraint (name "C") [TypeVar 0]] $ TypeVar 0)
            @?= "(C x) => x"
          show (TypeForall [0] [] $ TypeVar 0)
            @?= "forall v0 . v0"
      , testCase "unification rejects nested foralls conservatively" $ do
          let polymorphic = TypeForall [0] [] (TypeVar 0)
          unify polymorphic (TypeVar 1) @?= Nothing
          unifyRight (TypeVar 1) polymorphic @?= Nothing
      , testCase "symmetric unification closes substitutions across sides" $ do
          let pair pairParameter pairResult = TypeApp
                (TypeApp (TypeCons $ name "Pair") pairParameter) pairResult
              integer = TypeCons $ name "Int"
              left = pair (TypeVar 1) (TypeVar 1)
              right = pair (TypeVar 2) integer
          assertUnifierCloses left right
      , testCase "symmetric unification separates overlapping variable IDs" $ do
          let pair pairParameter pairResult = TypeApp
                (TypeApp (TypeCons $ name "Pair") pairParameter) pairResult
              maybeType value = TypeApp (TypeCons $ name "Maybe") value
              integer = TypeCons $ name "Int"
              left = pair (TypeVar 1) (TypeVar 2)
              right = pair (maybeType $ TypeVar 2) integer
          assertUnifierCloses left right
      , testCase "bounded symmetric unifiers satisfy their result contract" $ do
          let atoms =
                [ TypeVar 0
                , TypeVar 1
                , TypeVar 2
                , TypeCons $ name "Int"
                , TypeCons $ name "Bool"
                ]
              pair pairParameter pairResult = TypeApp
                (TypeApp (TypeCons $ name "Pair") pairParameter) pairResult
              types = atoms ++ [pair left right | left <- atoms, right <- atoms]
              pairs = [(left, right) | left <- types, right <- types]
          mapM_ (uncurry assertUnifierCloses) pairs
          mapM_ (uncurry $ assertOffsetUnifierCloses 20) pairs
      , testCase "failed synonym expansion preserves arguments" $ do
          let alias = name "Alias"
              argument = TypeCons (name "Argument")
          applyTypeDecls (Map.singleton alias $ Left "invalid declaration")
            (TypeApp (TypeCons alias) argument)
            @?= Right (TypeApp (TypeCons alias) argument)
      , testCase "type synonym cycles report their path" $ do
          let aliasA = name "A"
              aliasB = name "B"
              declarations = Map.fromList
                [ (aliasA, Right $ HsTypeDecl aliasA [] $ TypeCons aliasB)
                , (aliasB, Right $ HsTypeDecl aliasB [] $ TypeCons aliasA)
                ]
          applyTypeDecls declarations (TypeCons aliasA)
            @?= Left "cyclic type synonym: A -> B -> A"
      , testCase "unsaturated synonyms are rejected explicitly" $ do
          let alias = name "Alias"
              declaration = HsTypeDecl alias [0] (TypeVar 0)
          applyTypeDecls (Map.singleton alias $ Right declaration) (TypeCons alias)
            @?= Left "wrong number of parameters for type declaration Alias"
      , testCase "shared types round-trip flexible, rigid, tuple, and forall forms" $ do
          tuple <- expectRight $ mkBoxedTupleName 2
          let source = TypeForall [0]
                [HsConstraint (name "C") [TypeVar 0]]
                $ TypeArrow
                    (TypeApp
                      (TypeApp (TypeCons tuple) (TypeVar 0))
                      (TypeConstant 7))
                    (TypeApp (TypeCons ListCon) (TypeVar 0))
          shared <- expectRight $ toSynthesisType source
          SharedType.validateType shared @?= Right ()
          fromSynthesisType shared @?= Right source
      , testCase "shared type conversion rejects malformed class names" $ do
          let invalidName = name "constraint"
              source = TypeForall [0]
                [HsConstraint invalidName [TypeVar 0]]
                (TypeVar 0)
          toSynthesisType source @?= Left
            (InvalidSynthesisType $ SharedType.InvalidTypeConstraint
              $ SharedConstraint.InvalidConstraintClass
              $ toSynthesisName invalidName)
      , testCase "shared rigid forall binders are rejected" $ do
          let malformed = SharedType.ForallType
                [SharedType.RigidVariable 4] []
                (SharedType.TypeVariable $ SharedType.RigidVariable 4)
          fromSynthesisType malformed @?= Left (RigidForallBinder 4)
      ]
  , testGroup "shared declarations"
      [ testCase "function bindings preserve penalties and constraints" $ do
          let constraint = HsConstraint (name "C") [TypeVar 0]
              binding = FunctionBinding
                (TypeVar 0) (name "mapOne") (Penalty 2.5)
                [constraint] [TypeArrow (TypeVar 0) (TypeVar 0)]
          shared <- expectRight $ toSynthesisFunctionBinding binding
          fromSynthesisFunctionBinding shared @?= Right binding
      , testCase "classes and instances round-trip nominally" $ do
          let constraint = HsConstraint (name "C") [TypeVar 0]
              classDeclaration = HsTypeClass (name "C") [0] []
              instanceDeclaration = HsInstance [constraint] constraint
          sharedClass <- expectRight
            $ toSynthesisClassDeclaration classDeclaration
          fromSynthesisClassDeclaration sharedClass @?=
            Right classDeclaration
          sharedInstance <- expectRight
            $ toSynthesisInstanceDeclaration instanceDeclaration
          case sharedInstance of
            SharedDeclaration.InstanceDeclaration _ variables _ _ ->
              variables @?= [SharedType.FlexibleVariable 0]
            _ -> fail "instance adapter returned another declaration shape"
          fromSynthesisInstanceDeclaration sharedInstance @?=
            Right instanceDeclaration
      , testCase "deconstructor records preserve data shape and recursion" $ do
          let input = TypeApp (TypeCons $ name "Maybe") (TypeVar 0)
              declaration = DeconstructorBinding input
                [ ConstructorBinding (name "Nothing") []
                , ConstructorBinding (name "Just") [TypeVar 0]
                ] True
          shared <- expectRight $ toSynthesisDataDeclaration declaration
          fromSynthesisDataDeclaration shared @?= Right declaration
      , testCase "rated data declarations preserve constructor penalties" $ do
          let input = TypeApp (TypeCons $ name "Maybe") (TypeVar 0)
              nothing = ConstructorBinding (name "Nothing") []
              just = ConstructorBinding (name "Just") [TypeVar 0]
              declaration = DeconstructorBinding input [nothing, just] True
              penalties = Map.fromList
                [ (constructorName nothing, Penalty 1.5)
                , (constructorName just, Penalty 2.5)
                ]
              functions =
                [ FunctionBinding input (constructorName nothing)
                    (Penalty 1.5) [] []
                , FunctionBinding input (constructorName just)
                    (Penalty 2.5) [] [TypeVar 0]
                ]
          shared <- expectRight
            $ toSynthesisRatedDataDeclaration penalties declaration
          case shared of
            SharedDeclaration.DataTypeDeclaration _ _ _ constructors ->
              map SharedDeclaration.constructorAnnotation constructors @?=
                [ SearchPenaltyMetadata $ Penalty 1.5
                , SearchPenaltyMetadata $ Penalty 2.5
                ]
            _ -> fail "rated data adapter returned another declaration shape"
          fromSynthesisRatedDataDeclaration shared @?=
            Right (functions, declaration)
      , testCase "rated data declarations require complete penalties" $ do
          let missing = name "Just"
              declaration = DeconstructorBinding
                (TypeApp (TypeCons $ name "Maybe") (TypeVar 0))
                [ ConstructorBinding (name "Nothing") []
                , ConstructorBinding missing [TypeVar 0]
                ] False
          toSynthesisRatedDataDeclaration
              (Map.singleton (name "Nothing") $ Penalty 1)
              declaration
            @?= Left (MissingConstructorPenalty missing)
          shared <- expectRight $ toSynthesisDataDeclaration declaration
          fromSynthesisRatedDataDeclaration shared @?= Left
            (MissingSearchPenaltyMetadata $ toSynthesisName $ name "Nothing")
      , testCase "frontend type synonyms use the same declaration IR" $ do
          let declaration = HsTypeDecl (name "Pair") [0, 1]
                $ TypeApp
                  (TypeApp (TypeCons $ name "Tuple2") (TypeVar 0))
                  (TypeVar 1)
          shared <- expectRight $ toSynthesisTypeDeclaration declaration
          lowered <- expectRight $ fromSynthesisTypeDeclaration shared
          tdecl_name lowered @?= tdecl_name declaration
          tdecl_params lowered @?= tdecl_params declaration
          tdecl_result lowered @?= tdecl_result declaration
      , testCase "core environments round-trip without exporting inflated instances" $ do
          let cls = HsTypeClass (name "C") [0] []
              instanceDeclaration = HsInstance []
                $ HsConstraint (name "C") [TypeCons $ name "Int"]
              function = FunctionBinding
                (TypeCons $ name "Int") (name "answer") (Penalty 1) [] []
              input = TypeApp (TypeCons $ name "Maybe") (TypeVar 0)
              deconstructor = DeconstructorBinding input
                [ ConstructorBinding (name "Nothing") []
                , ConstructorBinding (name "Just") [TypeVar 0]
                ] False
          classes <- expectRight
            $ mkStaticClassEnv [cls] [instanceDeclaration]
          let source = EnvDictionary [function] [deconstructor] classes
          shared <- expectRight $ toSynthesisEnvironment source
          Map.keys (SharedEnvironment.valueSignatureMap shared)
            @?= [toSynthesisName $ name "answer"]
          Map.keys (SharedEnvironment.classDeclarationMap shared)
            @?= [toSynthesisName $ name "C"]
          Map.size (SharedEnvironment.instanceDeclarationMap shared) @?= 1
          lowered <- expectRight $ fromSynthesisEnvironment shared
          environmentFunctions lowered @?= [function]
          environmentDeconstructors lowered @?= [deconstructor]
          sClassEnv_tclasses (environmentClasses lowered)
            @?= Map.singleton (name "C") cls
          sClassEnv_explicitInstances (environmentClasses lowered)
            @?= [instanceDeclaration]
      , testCase "core lowering rejects frontend-only declarations" $ do
          let synonymName = toSynthesisName $ name "Alias"
              synonym = SharedDeclaration.TypeSynonymDeclaration
                NoDeclarationMetadata synonymName []
                (SharedType.TypeConstructor $ toSynthesisName $ name "Int")
          shared <- expectRight $ SharedEnvironment.mkEnvironment [synonym]
          case fromSynthesisEnvironment shared of
            Left (UnsupportedCoreEnvironmentDeclaration actual) ->
              actual @?= synonymName
            Left err -> fail $ "unexpected conversion error: " ++ show err
            Right _ -> fail "frontend-only type synonym reached the search core"
      , testCase "lossy declaration lowerings are rejected" $ do
          let parameter = SharedDeclaration.TypeParameter
                (SharedType.FlexibleVariable 0) Nothing
              method = SharedDeclaration.ValueSignature
                NoDeclarationMetadata (toSynthesisName $ name "method")
                (SharedType.TypeVariable $ SharedType.FlexibleVariable 0)
              sharedClass = SharedDeclaration.ClassDeclaration
                NoDeclarationMetadata (toSynthesisName $ name "C")
                [parameter] [] [method]
          fromSynthesisClassDeclaration sharedClass @?=
            Left (ClassMethodsUnsupported [toSynthesisName $ name "method"])
          toSynthesisDataDeclaration
              (DeconstructorBinding
                (TypeApp (TypeCons $ name "T") (TypeCons $ name "Int"))
                [] False)
            @?= Left (NonVariableDataParameter $ TypeCons $ name "Int")
          let unused = SharedType.FlexibleVariable 9
              sharedInstance = SharedDeclaration.InstanceDeclaration
                NoDeclarationMetadata [unused] []
                (SharedConstraint.Constraint
                  (toSynthesisName $ name "C") [])
          fromSynthesisInstanceDeclaration sharedInstance @?=
            Left (NonImplicitInstanceForall [unused])
      ]
  , testGroup "search policy"
      [ testCase "duplicate function names are rejected independently of order" $ do
          let duplicateName = name "f"
              intBinding = FunctionBinding
                (TypeCons $ name "Int") duplicateName 0 [] []
              boolBinding = FunctionBinding
                (TypeCons $ name "Bool") duplicateName 1 [] []
              expected = Left $ DuplicateFunctionNames [duplicateName]
          validateExferenceInput identityInput
            { input_envFuncs = [intBinding, boolBinding] } @?= expected
          validateExferenceInput identityInput
            { input_envFuncs = [boolBinding, intBinding] } @?= expected
      , testCase "duplicate datatype heads are rejected independently of order" $ do
          let duplicateName = name "T"
              firstDeconstructor = DeconstructorBinding
                (TypeApp (TypeCons duplicateName) $ TypeVar 0)
                [ConstructorBinding (name "First") [TypeVar 0]] False
              secondDeconstructor = DeconstructorBinding
                (TypeApp (TypeCons duplicateName) $ TypeVar 1)
                [ConstructorBinding (name "Second") [TypeVar 1]] False
              expected = Left $ DuplicateDeconstructorNames [duplicateName]
          validateExferenceInput identityInput
            { input_envDeconsS = [firstDeconstructor, secondDeconstructor] }
            @?= expected
          validateExferenceInput identityInput
            { input_envDeconsS = [secondDeconstructor, firstDeconstructor] }
            @?= expected
      , testCase "duplicate constructors are rejected independently of order" $ do
          let duplicateName = name "Shared"
              firstDeconstructor = DeconstructorBinding (TypeCons $ name "A")
                [ConstructorBinding duplicateName []] False
              secondDeconstructor = DeconstructorBinding
                (TypeApp (TypeCons $ name "B") $ TypeVar 0)
                [ConstructorBinding duplicateName [TypeVar 0]] False
              expected = Left $ DuplicateConstructorNames [duplicateName]
          validateExferenceInput identityInput
            { input_envDeconsS = [firstDeconstructor, secondDeconstructor] }
            @?= expected
          validateExferenceInput identityInput
            { input_envDeconsS = [secondDeconstructor, firstDeconstructor] }
            @?= expected
      , testCase "constraint relaxation ends at the configured step" $ do
          constraintsRelaxedAtStep False 2 1 @?= True
          constraintsRelaxedAtStep False 2 2 @?= True
          constraintsRelaxedAtStep False 2 3 @?= False
          constraintsRelaxedAtStep True 2 3 @?= True
      , testCase "known query constraints require their declared arity" $ do
          let cls = HsTypeClass (name "C") [0] []
              malformed = HsConstraint (name "C")
                [TypeVar 0, TypeVar 1]
          environment <- expectRight $ mkStaticClassEnv [cls] []
          validateExferenceInput identityInput
            { input_goalType = TypeForall [0]
                [] (TypeForall [1] [malformed] $ TypeVar 0)
            , input_envClasses = environment
            }
            @?= Left (InvalidClassConstraint $ ConstraintArityMismatch
              QueryConstraint (name "C") 1 2)
      , testCase "known binding constraints require their declared arity" $ do
          let cls = HsTypeClass (name "C") [0] []
              malformed = HsConstraint (name "C") []
              bindingName = name "f"
              binding = FunctionBinding (TypeVar 0) bindingName 0
                [malformed] []
          environment <- expectRight $ mkStaticClassEnv [cls] []
          validateExferenceInput identityInput
            { input_envFuncs = [binding]
            , input_envClasses = environment
            }
            @?= Left (InvalidClassConstraint $ ConstraintArityMismatch
              (BindingConstraint bindingName) (name "C") 1 0)
      , testCase "unknown query classes remain nominal external constraints" $
          validateExferenceInput identityInput
            { input_goalType = TypeForall [0]
                [HsConstraint (name "External") [TypeVar 0]]
                (TypeVar 0)
            }
            @?= Right ()
      , testCase "unknown query class names still use the class namespace" $
          let invalidName = name "external"
          in validateExferenceInput identityInput
              { input_goalType = TypeForall [0]
                  [HsConstraint invalidName [TypeVar 0]]
                  (TypeVar 0)
              }
              @?= Left (InvalidClassConstraint $ InvalidClassName invalidName)
      , testCase "unknown binding classes remain nominal external constraints" $
          let binding = FunctionBinding (TypeVar 0) (name "f") 0
                [HsConstraint (name "External") [TypeVar 0]] []
          in validateExferenceInput identityInput
              { input_envFuncs = [binding] }
              @?= Right ()
      , testCase "unknown binding class names still use the class namespace" $
          let invalidName = name "external"
              binding = FunctionBinding (TypeVar 0) (name "f") 0
                [HsConstraint invalidName [TypeVar 0]] []
          in validateExferenceInput identityInput
              { input_envFuncs = [binding] }
              @?= Left (InvalidClassConstraint $ InvalidClassName invalidName)
      , testCase "nested foralls return a structured input error" $ do
          let polymorphic = TypeForall [0] [] (TypeVar 0)
              goal = TypeArrow polymorphic polymorphic
              input = ExferenceInput goal [] [] emptyClassEnv
                False False 0 False 20 Nothing Nothing defaultHeuristicsConfig
          case findExpressionsEither input of
            Left actual -> actual @?= NestedForallInGoal goal
            Right _ -> fail "nested forall was accepted"
          case findExpressionsWithStatsEither input of
            Left actual -> actual @?= NestedForallInGoal goal
            Right _ -> fail "the checked chunk API discarded validation failure"
          let nestedConstraint = HsConstraint (name "Inner") [TypeVar 1]
              outerConstraint = HsConstraint (name "Outer")
                [TypeForall [1] [nestedConstraint] $ TypeVar 1]
              constrainedGoal = TypeForall [0] [outerConstraint] $ TypeVar 0
          typeConstraints constrainedGoal
            @?= [outerConstraint, nestedConstraint]
          validateExferenceInput input { input_goalType = constrainedGoal }
            @?= Left (NestedForallInGoal constrainedGoal)
      , testCase "prenex forall chains are checked through every layer" $ do
          let goal = TypeForall [0] []
                $ TypeForall [1] []
                $ TypeArrow (TypeVar 1) (TypeVar 1)
              identity = ExpLambda 1 (TypeConstant 1)
                (ExpVar 1 $ TypeConstant 1)
              input = ExferenceInput goal [] [] emptyClassEnv
                False False 0 False 20 Nothing Nothing defaultHeuristicsConfig
          validateExferenceInput input @?= Right ()
          checkExpression (mkQueryClassEnv emptyClassEnv []) [] []
            goal [] identity @?= Right ()
          case findOneExpression input of
            Just _ -> pure ()
            Nothing -> fail "prenex forall identity was filtered after search"
      , testCase "step exhaustion is reported explicitly" $ do
          chunk <- onlyChunk $ identityInput {input_maxSteps = 1}
          searchCompletion (chunkStatus chunk) @?= SearchStepLimitReached
          toSearchProgress (chunkStatus chunk) @?= Right
            (SharedSearch.Completed $ SharedSearch.truncated
              SharedSearch.StepLimitReached)
      , testCase "complete failure is distinguished from bounded search" $ do
          chunk <- lastChunk $ identityInput
            {input_goalType = TypeCons $ name "Void"}
          assertBool "an uninhabited atomic goal produced an expression"
            $ null $ chunkElements chunk
          chunkStatus chunk @?= SearchStatus SearchExhausted 0 0
          toSearchProgress (chunkStatus chunk) @?=
            Right (SharedSearch.Completed SharedSearch.Finished)
          case toSearchBatch chunk of
            Left batchError -> fail $ show batchError
            Right batch -> assertBool
              "common batch unexpectedly gained candidates"
              $ null $ SharedSearch.batchCandidates batch
      , testCase "queue pruning is bounded and reported" $ do
          chunk <- onlyChunk $ identityInput {input_maxQueueSize = Just 0}
          searchCompletion (chunkStatus chunk) @?= SearchPruned
          searchQueuePruned (chunkStatus chunk) @?= 1
          toSearchProgress (chunkStatus chunk) @?= Right
            (SharedSearch.Completed $ SharedSearch.truncated
              $ SharedSearch.QueueLimitPruned 1)
      , testCase "depth pruning is configured and reported" $ do
          let config = defaultHeuristicsConfig
                {heuristics_functionGoalTransform = 1}
          chunk <- onlyChunk $ identityInput
            { input_maxDepth = Just 0
            , input_heuristicsConfig = config
            }
          searchCompletion (chunkStatus chunk) @?= SearchPruned
          searchDepthPruned (chunkStatus chunk) @?= 1
          toSearchProgress (chunkStatus chunk) @?= Right
            (SharedSearch.Completed $ SharedSearch.truncated
              $ SharedSearch.DepthLimitPruned 1)
      , testCase "status-aware selectors preserve policy semantics" $ do
          let status index = SearchStatus SearchRunning index 0
              terminal = SearchStatus SearchStepLimitReached 6 0
              constrained = [HsConstraint (name "C") []]
              candidate index rating constraints =
                ( ExpHole index
                , constraints
                , ExferenceStats index (Penalty rating) 0
                )
              chunk chunkStatus' elements = ExferenceChunkElement
                chunkStatus' Map.empty elements
              chunks =
                [ chunk (status 1) [candidate 1 4 constrained]
                , chunk (status 2) []
                , chunk (status 3)
                    [ candidate 2 3 constrained
                    , candidate 3 2 []
                    , candidate 4 2 []
                    ]
                , chunk (status 4) []
                , chunk (status 5) [candidate 5 5 []]
                , chunk terminal []
                ]
              chunksWithLateConstraint = take 3 chunks ++
                [ chunk (status 4) [candidate 6 9 constrained]
                , chunk (status 5) [candidate 5 5 []]
                , chunk terminal []
                ]
              steps = map (exference_steps . third) . selectionResult
              third (_, _, value) = value
          steps (selectOneExpression chunks) @?= [1]
          selectionStatus (selectOneExpression chunks) @?= Just (status 1)
          steps (selectSortNExpressions 4 chunks) @?= [3, 4, 2, 1]
          selectionStatus (selectSortNExpressions 4 chunks)
            @?= Just (status 3)
          steps (selectBestNExpressions 4 chunks) @?= [3, 4]
          steps (selectFirstBestExpressions chunks) @?= [4, 3]
          selectionStatus (selectFirstBestExpressions chunks)
            @?= Just (status 5)
          steps (selectFirstExpressionLookahead 2 chunks) @?= [3]
          selectionStatus (selectFirstExpressionLookahead 2 chunks)
            @?= Just (status 5)
          steps (selectFirstBestExpressionsLookahead 3 chunks) @?= [4, 3]
          selectionStatus (selectFirstBestExpressionsLookahead 3 chunks)
            @?= Just (status 5)
          steps (selectFirstBestExpressionsLookahead 0 chunks) @?= [1]
          selectionStatus (selectFirstBestExpressionsLookahead 0 chunks)
            @?= Just (status 1)
          steps (selectFirstBestExpressionsLookaheadPreferNoConstraints 3 chunks)
            @?= [4, 3]
          selectionStatus
              (selectFirstBestExpressionsLookaheadPreferNoConstraints 3 chunks)
            @?= Just (status 5)
          steps
              (selectFirstBestExpressionsLookaheadPreferNoConstraints
                4 chunksWithLateConstraint)
            @?= [4, 3]
          selectionStatus
              (selectFirstBestExpressionsLookaheadPreferNoConstraints
                4 chunksWithLateConstraint)
            @?= Just (status 5)
      , testCase "empty selectors retain the terminal search status" $ do
          let running = SearchStatus SearchRunning 0 0
              pruned = SearchStatus SearchPruned 7 2
              chunks =
                [ ExferenceChunkElement running Map.empty []
                , ExferenceChunkElement pruned Map.empty []
                ]
              assertEmpty selection = do
                assertBool "empty trace unexpectedly selected an expression"
                  $ null $ selectionResult selection
                selectionStatus selection @?= Just pruned
          assertEmpty $ selectOneExpression chunks
          assertEmpty $ selectBestNExpressions 4 chunks
          assertEmpty $ selectFirstBestExpressionsLookahead 4 chunks
          assertEmpty $
            selectFirstBestExpressionsLookaheadPreferNoConstraints 4 chunks
          toSearchProgress pruned @?= Right
            (SharedSearch.Completed $ SharedSearch.Truncated
              (SharedSearch.QueueLimitPruned 7 :|
                [SharedSearch.DepthLimitPruned 2]))
      , testCase "malformed compatibility statuses are rejected" $ do
          toSearchProgress (SearchStatus SearchPruned 0 0) @?=
            Left PrunedWithoutDiscardedNodes
          toSearchProgress (SearchStatus SearchPruned (-1) 0) @?=
            Left (NegativeQueuePruningCount (-1))
      , testCase "non-finite heuristic inputs are rejected" $ do
          let config = defaultHeuristicsConfig
                {heuristics_goalVar = Penalty (0 / 0)}
          case findExpressionsEither identityInput
              {input_heuristicsConfig = config} of
            Left (InvalidHeuristic "goalVar" (Penalty value)) ->
              isNaN value @?= True
            Left other -> fail $ "unexpected validation error: " ++ show other
            Right _ -> fail "non-finite heuristic was accepted"
      , testCase "signed finite function ratings are accepted" $ do
          let variable = TypeVar 0
              binding = FunctionBinding
                { functionResult = variable
                , functionName = name "preferred"
                , functionPenalty = Penalty (-3.5)
                , functionConstraints = []
                , functionParameters = [variable]
                }
          case findExpressionsEither identityInput
              {input_envFuncs = [binding]} of
            Left err -> fail $ "signed rating was rejected: " ++ show err
            Right _ -> pure ()
      ]
  , testGroup "validated names"
      [ testCase "legacy bundled constructors remain import-compatible" $
          map show CompatibilityImport.legacyConstructorValues
            @?= ["id", "[]", "()", "(:)"]
      , testCase "checked ordinary construction canonicalizes operators" $ do
          operator <- expectRight
            $ mkQualifiedName ["Control", "Applicative"] "(<*>)"
          show operator @?= "Control.Applicative.(<*>)"
          qualifiedNameOperator operator @?= Just "<*>"
          function <- expectRight $ mkQualifiedName [] "->"
          qualifiedNameOperator function @?= Just "->"
          function @?= QualifiedName [] "->"
      , testCase "checked ordinary construction rejects contextual syntax" $
          mapM_ (assertNameRejected . uncurry mkQualifiedName)
            [ ([], " map")
            , ([], "map ")
            , ([], "`map`")
            , (["Data"], "Data.map")
            ]
      , testCase "shared conversion rejects unboxed tuples" $ do
          shared <- expectRight $ SharedName.tupleName SharedName.Unboxed 2
          fromSynthesisName shared @?= Left
            (UnsupportedSpecialName
              $ SharedName.TupleConstructor SharedName.Unboxed 2)
      , testCase "shared conversion round-trips the Exference subset" $ do
          operator <- expectRight $ mkQualifiedName ["Data", "Function"] "."
          tuple <- expectRight $ mkBoxedTupleName 3
          mapM_ (\qualifiedName ->
              fromSynthesisName (toSynthesisName qualifiedName)
                @?= Right qualifiedName)
            [operator, tuple, ListCon, Cons, QualifiedName [] "->"]
      , testCase "Eq, Ord, Show, Generic, and NFData observe one value" $ do
          value <- expectRight $ mkQualifiedName ["Data", "List"] "map"
          value @?= value
          compare value value @?= EQ
          show value @?= "Data.List.map"
          Generic.to (Generic.from value) @?= value
          force value @?= value
      , testCase "Data retains the historical four-constructor view" $ do
          ordinary <- expectRight $ mkQualifiedName ["Data", "List"] "map"
          tuple <- expectRight $ mkBoxedTupleName 3
          map (showConstr . toConstr) [ordinary, ListCon, tuple, Cons]
            @?= ["QualifiedName", "ListCon", "TupleCon", "Cons"]
          map (length . gmapQ (const ())) [ordinary, ListCon, tuple, Cons]
            @?= [2, 0, 1, 0]
          map showConstr (dataTypeConstrs $ dataTypeOf ordinary)
            @?= ["QualifiedName", "ListCon", "TupleCon", "Cons"]
          dataTypeName (dataTypeOf ordinary)
            @?= "Language.Haskell.Exference.Core.Types.QualifiedName"
          let constructors = dataTypeConstrs $ dataTypeOf ordinary
          (fromConstr (constructors !! 1) :: QualifiedName) @?= ListCon
          (fromConstr (constructors !! 3) :: QualifiedName) @?= Cons
      ]
  , testGroup "parsing and diagnostics"
      [ testCase "ratings reject a missing value" $
          first diagnosticMessage (parseRatings "foo") @?= Left
            "rating file ends with a name but no numeric rating"
      , testCase "ratings reject a malformed number" $
          first diagnosticMessage (parseRatings "foo nope")
            @?= Left "invalid rating for foo: nope"
      , testCase "ratings reject non-finite values" $
          first diagnosticMessage (parseRatings "foo NaN")
            @?= Left "rating for foo must be finite: NaN"
      , testCase "missing modules produce source-bearing read errors" $ do
          environmentDirectory <- getDataFileName "environment"
          let modulePath = environmentDirectory ++ "/missing-module.hs"
              ratingPath = environmentDirectory ++ "/all.ratings"
          (result, messages :: [String]) <- runMultiRWSTNil
            $ withMultiWriterAW
            $ environmentFromModuleAndRatings modulePath ratingPath
          case result of
            Left (ModuleReadErrors (readError :| remainingErrors)) -> do
              diagnosticSource readError @?= Just modulePath
              remainingErrors @?= []
            Left failure -> fail $ "unexpected load failure: " ++ show failure
            Right _ -> fail "a missing module was accepted"
          assertBool ("later loader summaries survived: " ++ show messages)
            $ not $ any isLoaderSummary messages
      , testCase "missing environment directories produce source-bearing errors" $ do
          environmentDirectory <- getDataFileName "environment"
          let missingDirectory = environmentDirectory ++ "/missing-environment"
          (result, messages :: [String]) <- runMultiRWSTNil
            $ withMultiWriterAW
            $ environmentFromPath missingDirectory
          case result of
            Left (EnvironmentDirectoryReadError readError) ->
              diagnosticSource readError @?= Just missingDirectory
            Left failure -> fail $ "unexpected load failure: " ++ show failure
            Right _ -> fail "a missing environment directory was accepted"
          assertBool ("failed directory load emitted messages: " ++ show messages)
            $ null messages
      , testCase "missing ratings retain zero penalties with one warning" $ do
          environmentDirectory <- getDataFileName "environment"
          let modulePath = environmentDirectory ++ "/Category.hs"
              ratingPath = environmentDirectory ++ "/missing.ratings"
          (result, messages :: [String]) <- runMultiRWSTNil
            $ withMultiWriterAW
            $ environmentFromModuleAndRatings modulePath ratingPath
          checkedEnvironment <- expectRight result
          let bindings = sourceFunctions
                $ checkedSourceProjection checkedEnvironment
              warnings = filter (not . isLoaderSummary) messages
          assertBool "missing ratings changed a default function penalty"
            $ all ((== Penalty 0) . functionPenalty) bindings
          case warnings of
            [warning] -> assertBool
              ("rating warning lost its source: " ++ show warning)
              $ ratingPath `isInfixOf` warning
            _ -> fail $ "expected one rating warning, got " ++ show warnings
      , testCase "duplicate ratings are diagnosed and ignored" $
          withTemporaryFile (unlines
            [ "module Ratings where"
            , "identity :: a -> a"
            ]) $ \modulePath ->
          withTemporaryFile
            "Ratings.identity 1.0 Ratings.identity 2.0"
            $ \ratingPath -> do
              (result, messages :: [String]) <- runMultiRWSTNil
                $ withMultiWriterAW
                $ environmentFromModuleAndRatings modulePath ratingPath
              environment <- checkedSourceProjection <$> expectRight result
              identityName <- expectRight
                $ mkQualifiedName ["Ratings"] "identity"
              case find ((== identityName) . functionName)
                  (sourceFunctions environment) of
                Nothing -> fail "the rated identity declaration was lost"
                Just binding -> functionPenalty binding @?= Penalty 0
              assertBool ("duplicate rating was not diagnosed: " ++ show messages)
                $ "duplicate rating: Ratings.identity" `elem` messages
      , testCase "malformed modules fail before loader summaries" $ do
          environmentDirectory <- getDataFileName "environment"
          let modulePath = environmentDirectory ++ "/all.ratings"
          (result, messages :: [String]) <- runMultiRWSTNil
            $ withMultiWriterAW
            $ parseModules [(haskellSrcExtsParseMode modulePath, modulePath)]
          case result of
            Left ModuleParseErrors{} -> pure ()
            Left failure -> fail $ "unexpected load failure: " ++ show failure
            Right _ -> fail "a malformed module was accepted"
          assertBool ("later loader summaries survived: " ++ show messages)
            $ not $ any isLoaderSummary messages
      , testCase "malformed ratings retain the parsed environment at defaults" $ do
          environmentDirectory <- getDataFileName "environment"
          let modulePath = environmentDirectory ++ "/Category.hs"
          (sourceEnvironmentResult, messages :: [String]) <-
            runMultiRWSTNil $ withMultiWriterAW
              $ environmentFromModuleAndRatings modulePath modulePath
          checkedEnvironment <- expectRight sourceEnvironmentResult
          let sourceEnvironment = checkedSourceProjection checkedEnvironment
          categoryName <- expectRight
            $ mkQualifiedName ["Control", "Category"] "Category"
          identityName <- expectRight
            $ mkQualifiedName ["Control", "Category"] "id"
          assertBool "malformed ratings discarded parsed deconstructors"
            $ not $ null $ sourceDeconstructors sourceEnvironment
          Map.member categoryName
            (sClassEnv_tclasses $ sourceClasses sourceEnvironment) @?= True
          assertBool "malformed ratings discarded parsed class names"
            $ categoryName `elem` sourceTypeNames sourceEnvironment
          case find ((== identityName) . functionName)
              (sourceFunctions sourceEnvironment) of
            Nothing -> fail "malformed ratings discarded parsed declarations"
            Just binding -> functionPenalty binding @?= Penalty 0
          assertBool ("missing rating diagnostic: " ++ show messages)
            $ any ("could not parse rating file" `isInfixOf`) messages
      , testCase "frontend source environments retain type synonyms" $ do
          environmentDirectory <- getDataFileName "environment"
          let modulePath = environmentDirectory ++ "/String.hs"
          (sourceEnvironmentResult, _ :: [String]) <- runMultiRWSTNil
            $ withMultiWriterAW
            $ environmentFromModuleAndRatings modulePath modulePath
          checkedEnvironment <- expectRight sourceEnvironmentResult
          let sourceEnvironment = checkedSourceProjection checkedEnvironment
          shared <- expectRight
            $ toSynthesisSourceEnvironment sourceEnvironment
          stringName <- expectRight
            $ mkQualifiedName ["Data", "String"] "String"
          case Map.lookup (toSynthesisName stringName)
              (SharedEnvironment.typeDeclarationMap shared) of
            Just SharedDeclaration.TypeSynonymDeclaration{} -> pure ()
            declaration -> fail $
              "source synonym missing from shared environment: "
                ++ show declaration
          Map.member SharedName.listName
            (SharedEnvironment.typeDeclarationMap shared) @?= True
          Map.member SharedName.consName
            (SharedEnvironment.dataConstructorMap shared) @?= True
          Map.member SharedName.consName
            (SharedEnvironment.valueSignatureMap shared) @?= False
      , testCase "duplicate synonyms reach the shared inventory in source order" $ do
          parsedModule <- expectParsedModule $ unlines
            [ "module M where"
            , "type Alias = Int"
            , "type Alias = Bool"
            ]
          let results = runIdentity $ runMultiRWSTNil $
                getTypeDecls [] [parsedModule]
              synonyms = rights results
          aliasName <- expectRight $ mkQualifiedName ["M"] "Alias"
          map tdecl_name synonyms @?= [aliasName, aliasName]
          let environment :: SourceEnvironment FunctionBinding
              environment = SourceEnvironment
                { sourceFunctions = []
                , sourceDeconstructors = []
                , sourceClasses = emptyStaticClassEnv
                , sourceTypeNames = [aliasName]
                , sourceTypeSynonyms = synonyms
                }
          toSynthesisSourceInventory environment @?= Left
            (InvalidSharedEnvironment
              $ SharedEnvironment.DuplicateTypeDeclaration
              $ toSynthesisName aliasName)
      , testCase "source inventories require every constructor function" $ do
          let missing = map name ["Just", "Nothing"]
              environment = maybeLikeSourceEnvironment
                { sourceFunctions = filter
                    ((`notElem` missing) . functionName)
                    $ sourceFunctions maybeLikeSourceEnvironment
                }
          toSynthesisSourceInventory environment @?= Left
            (MissingConstructorFunctionBindings
              $ map toSynthesisName missing)
      , testCase "source inventories reject duplicate constructor functions" $ do
          let duplicate = map name ["Just", "Nothing"]
              result = TypeApp (TypeCons $ name "Maybe") (TypeVar 0)
              duplicateBindings =
                [ FunctionBinding result (name "Just")
                    (Penalty 2.75) [] [TypeVar 0]
                , FunctionBinding result (name "Nothing")
                    (Penalty 1.25) [] []
                ]
              environment = maybeLikeSourceEnvironment
                { sourceFunctions = duplicateBindings
                    ++ sourceFunctions maybeLikeSourceEnvironment
                }
          toSynthesisSourceInventory environment @?= Left
            (DuplicateConstructorFunctionBindings
              $ map toSynthesisName duplicate)
      , testCase "source inventories reject orphan constructor functions" $ do
          let orphans = map name ["Another", "Orphan"]
              result = TypeApp (TypeCons $ name "Maybe") (TypeVar 0)
              environment = maybeLikeSourceEnvironment
                { sourceFunctions =
                    [ FunctionBinding result orphan (Penalty 4) [] []
                    | orphan <- orphans
                    ] ++ sourceFunctions maybeLikeSourceEnvironment
                }
          toSynthesisSourceInventory environment @?= Left
            (OrphanConstructorBindings $ map toSynthesisName orphans)
      , testCase "source inventories reject mismatched constructor shapes" $ do
          let mismatched = map name ["Just", "Nothing"]
              changeShape binding
                | functionName binding == name "Just" = binding
                    { functionParameters = [] }
                | functionName binding == name "Nothing" = binding
                    { functionParameters = [TypeVar 0] }
                | otherwise = binding
              environment = maybeLikeSourceEnvironment
                { sourceFunctions = map changeShape
                    $ sourceFunctions maybeLikeSourceEnvironment
                }
          toSynthesisSourceInventory environment @?= Left
            (MismatchedConstructorFunctionBindings
              $ map toSynthesisName mismatched)
      , testCase "source inventories retain constructor and value penalties" $ do
          inventory <- expectRight
            $ toSynthesisSourceInventory maybeLikeSourceEnvironment
          let shared = SharedInventory.inventoryEnvironment inventory
              constructors = SharedEnvironment.dataConstructorMap shared
              values = SharedEnvironment.valueSignatureMap shared
              constructorPenalty constructor =
                SharedDeclaration.constructorAnnotation
                  <$> Map.lookup (toSynthesisName $ name constructor)
                    constructors
              valuePenalty value =
                SharedDeclaration.valueAnnotation
                  <$> Map.lookup (toSynthesisName $ name value) values
          constructorPenalty "Nothing" @?=
            Just (SearchPenaltyMetadata $ Penalty 1.25)
          constructorPenalty "Just" @?=
            Just (SearchPenaltyMetadata $ Penalty 2.75)
          valuePenalty "defaultMaybe" @?=
            Just (SearchPenaltyMetadata $ Penalty 3.5)
      , testCase "the shipped source environment seals as one inventory" $ do
          environmentDirectory <- getDataFileName "environment"
          (sourceEnvironmentResult, _ :: [String]) <- runMultiRWSTNil
            $ withMultiWriterAW $ environmentFromPath environmentDirectory
          checkedEnvironment <- expectRight sourceEnvironmentResult
          let inventory = checkedSourceInventory checkedEnvironment
              shared = SharedInventory.inventoryEnvironment inventory
          assertBool "shared source inventory lost type declarations"
            $ not $ Map.null $ SharedEnvironment.typeDeclarationMap shared
          assertBool "shared source inventory lost values"
            $ not $ Map.null $ SharedEnvironment.valueSignatureMap shared
          maybeName <- expectRight
            $ mkQualifiedName ["Data", "Maybe"] "Maybe"
          Map.lookup (toSynthesisName maybeName)
              (SharedKindInference.typeConstructorKinds
                $ SharedInventory.inventoryKindAssumptions inventory) @?=
            Just (SharedKind.FunctionKind
              SharedKind.ProperTypeKind SharedKind.ProperTypeKind)
      , testCase "built-in constructors retain configured search penalties" $ do
          environmentDirectory <- getDataFileName "environment"
          (sourceEnvironmentResult, _ :: [String]) <- runMultiRWSTNil
            $ withMultiWriterAW $ environmentFromPath environmentDirectory
          checkedEnvironment <- expectRight sourceEnvironmentResult
          let constructors = SharedEnvironment.dataConstructorMap
                $ SharedInventory.inventoryEnvironment
                $ checkedSourceInventory checkedEnvironment
              constructorPenalty constructor =
                SharedDeclaration.constructorAnnotation
                  <$> Map.lookup (toSynthesisName constructor) constructors
          mapM_ (\(constructor, expectedPenalty) ->
              constructorPenalty constructor @?=
                Just (SearchPenaltyMetadata expectedPenalty))
            [ (ListCon, Penalty 0)
            , (Cons, Penalty 5)
            , (TupleCon 0, Penalty 9.9)
            , (TupleCon 2, Penalty 5)
            , (TupleCon 3, Penalty 5)
            , (TupleCon 4, Penalty 4)
            , (TupleCon 5, Penalty 3)
            , (TupleCon 6, Penalty 2)
            , (TupleCon 7, Penalty 0)
            ]
      , testCase "source environments reject ill-kinded signatures" $ do
          let intName = name "Int"
              badName = name "bad"
              environment = SourceEnvironment
                { sourceFunctions =
                    [ FunctionBinding
                        (TypeApp (TypeCons intName) (TypeCons intName))
                        badName (Penalty 0) [] []
                    ]
                , sourceDeconstructors =
                    [DeconstructorBinding (TypeCons intName) [] False]
                , sourceClasses = emptyStaticClassEnv
                , sourceTypeNames = [intName]
                , sourceTypeSynonyms = []
                }
              proper = SharedKind.ProperTypeKind
          toSynthesisSourceEnvironment environment @?= Left
            (InvalidSourceEnvironmentKinds
              (SharedKindInference.DeclarationKindError
                (toSynthesisName badName)
                (SharedKindInference.KindMismatch proper
                  $ SharedKind.FunctionKind proper proper)))
      , testCase "loader names unknown type constructors precisely" $ do
          withTemporaryFile (unlines
            [ "module Warnings where"
            , "external :: Data.External.External"
            ]) $ \modulePath -> do
              (result, messages :: [String]) <- runMultiRWSTNil
                $ withMultiWriterAW
                $ parseModules
                    [(haskellSrcExtsParseMode modulePath, modulePath)]
              _ <- expectRight result
              assertBool
                ("missing type-constructor diagnostic: " ++ show messages)
                $ "unknown type constructor 'Data.External.External' used in the binding Warnings.external"
                    `elem` messages
              assertBool ("legacy diagnostic survived: " ++ show messages)
                $ not $ any ("unknown binding" `isInfixOf`) messages
      , testCase "loader validates signature classes against its inventory" $ do
          withTemporaryFile (unlines
            [ "module Warnings where"
            , "constrained :: External.Constraint a => a -> a"
            ]) $ \modulePath -> do
              (result, messages :: [String]) <- runMultiRWSTNil
                $ withMultiWriterAW
                $ parseModules
                    [(haskellSrcExtsParseMode modulePath, modulePath)]
              _ <- expectRight result
              assertBool
                ("missing constraint-class diagnostic: " ++ show messages)
                $ "unknown constraint class 'External.Constraint' used in the binding Warnings.constrained"
                    `elem` messages
      , testCase "partial class inventories fail before advisory warnings" $ do
          environmentDirectory <- getDataFileName "environment"
          let paths = map ((environmentDirectory ++ "/") ++)
                ["Eq.hs", "Ord.hs"]
              warning = "unknown type constructor 'Data.Monoid.Last' used in class instances"
          (result, (messages :: [String])) <- runMultiRWSTNil
            $ withMultiWriterAW $ parseModules
            [(haskellSrcExtsParseMode path, path) | path <- paths]
          case result of
            Left (ClassEnvironmentLoadFailure _) -> pure ()
            Left failure -> fail $ "unexpected load failure: " ++ show failure
            Right _ -> fail "an incomplete class inventory was accepted"
          length (filter (== warning) messages) @?= 0
          assertBool ("later loader summaries survived: " ++ show messages)
            $ not $ any isLoaderSummary messages
      , testCase "qualified names reject empty path segments" $
          case parseQualifiedName "Data..map" of
            Left _ -> pure ()
            Right result -> fail $ "malformed name was accepted: " ++ show result
      , testCase "qualified operators retain module and spelling" $
          parseQualifiedName "Control.Applicative.(<*>)"
            @?= Right (QualifiedName ["Control", "Applicative"] "<*>")
      , testCase "broad unqualified lookup diagnoses ambiguity" $ do
          typeA <- expectRight $ mkQualifiedName ["A"] "T"
          typeB <- expectRight $ mkQualifiedName ["B"] "T"
          let syntaxName = HSE.UnQual HSE.noSrcSpan
                (HSE.Ident HSE.noSrcSpan "T")
          case convertQName Nothing [typeA, typeB] syntaxName of
            Left message -> do
              assertBool message $ "ambiguous unqualified name" `isInfixOf` message
              assertBool message $ "A.T" `isInfixOf` message
              assertBool message $ "B.T" `isInfixOf` message
            Right result -> fail $ "ambiguous name resolved as " ++ show result
      , testCase "module lookup prefers a local declaration" $ do
          local <- expectRight $ mkQualifiedName ["Current"] "C"
          imported <- expectRight $ mkQualifiedName ["Imported"] "C"
          let syntaxName = HSE.UnQual HSE.noSrcSpan
                (HSE.Ident HSE.noSrcSpan "C")
              currentModule = HSE.ModuleName HSE.noSrcSpan "Current"
          convertQName (Just currentModule) [imported, local] syntaxName
            @?= Right local
      , testCase "module lookup resolves a unique imported occurrence" $ do
          imported <- expectRight $ mkQualifiedName ["Imported"] "C"
          let syntaxName = HSE.UnQual HSE.noSrcSpan
                (HSE.Ident HSE.noSrcSpan "C")
              currentModule = HSE.ModuleName HSE.noSrcSpan "Current"
          convertQName (Just currentModule) [imported] syntaxName
            @?= Right imported
      , testCase "module lookup preserves an unknown external occurrence" $ do
          external <- expectRight $ mkQualifiedName [] "C"
          let syntaxName = HSE.UnQual HSE.noSrcSpan
                (HSE.Ident HSE.noSrcSpan "C")
              currentModule = HSE.ModuleName HSE.noSrcSpan "Current"
          convertQName (Just currentModule) [] syntaxName
            @?= Right external
      , testCase "module lookup diagnoses ambiguous imported occurrences" $ do
          importedA <- expectRight $ mkQualifiedName ["A"] "C"
          importedB <- expectRight $ mkQualifiedName ["B"] "C"
          let syntaxName = HSE.UnQual HSE.noSrcSpan
                (HSE.Ident HSE.noSrcSpan "C")
              currentModule = HSE.ModuleName HSE.noSrcSpan "Current"
          case convertQName (Just currentModule)
              [importedA, importedB] syntaxName of
            Left message -> assertBool message
              $ "ambiguous unqualified name" `isInfixOf` message
            Right result -> fail $ "ambiguous import resolved as " ++ show result
      , testCase "qualified operators render canonically and round-trip" $ do
          let names =
                [ QualifiedName [] "."
                , QualifiedName ["Control", "Applicative"] "<*>"
                , QualifiedName ["Control", "Monad"] ">>="
                , QualifiedName [] "⊕"
                , QualifiedName ["Math", "Operators"] "⊕"
                , QualifiedName [] "->"
                , ListCon
                , TupleCon 0
                , Cons
                ] ++ map TupleCon [2 .. 7]
          show (QualifiedName ["Control", "Applicative"] "<*>")
            @?= "Control.Applicative.(<*>)"
          show (QualifiedName ["Math", "Operators"] "⊕")
            @?= "Math.Operators.(⊕)"
          mapM_ (\qualifiedName ->
              parseQualifiedName (show qualifiedName) @?= Right qualifiedName)
            names
      , testCase "rating names recover structural built-in constructors" $ do
          let source = unlines
                [ "[] 1"
                , "() 2"
                , "(:) 3"
                , "(,) 4"
                , "(,,) 5"
                , "(->) 6"
                ]
          fmap (map fst) (parseRatings source) @?= Right
            [ ListCon
            , TupleCon 0
            , Cons
            , TupleCon 2
            , TupleCon 3
            , QualifiedName [] "->"
            ]
      , testCase "operator ratings apply by structural name equality" $ do
          let operator = QualifiedName ["Control", "Applicative"] "<*>"
              ty = TypeArrow (TypeVar 0) (TypeVar 0)
          ratings <- expectRight
            $ parseRatings "Control.Applicative.(<*>) 0.3"
          compileWithDict ratings [(operator, ty)]
            @?= Right [(operator, Penalty 0.3, ty)]
      , testCase "the shipped rating file parses and every name round-trips" $ do
          environmentDirectory <- getDataFileName "environment"
          contents <- readFile $ environmentDirectory ++ "/all.ratings"
          ratings <- expectRight $ parseRatings contents
          assertBool "the shipped rating file unexpectedly parsed no entries"
            (not $ null ratings)
          mapM_ (\(qualifiedName, _) ->
              parseQualifiedName (show qualifiedName) @?= Right qualifiedName)
            ratings
      , testCase "the built-in unit value inhabits parsed unit" $ do
          environmentDirectory <- getDataFileName "environment"
          (bindings, classEnvironment, messages) <-
            loadEnvironmentAndMessages environmentDirectory
          let unitBindings = filter ((== TupleCon 0) . functionName) bindings
              applicativeOperator = QualifiedName
                ["Control", "Applicative"] "<*>"
          case find ((== applicativeOperator) . functionName) bindings of
            Nothing -> fail "the shipped Applicative operator was not loaded"
            Just binding -> functionPenalty binding @?= Penalty 0.3
          mapM_ (\(constructor, expectedPenalty) ->
              case find ((== constructor) . functionName) bindings of
                Nothing -> fail $ "built-in constructor was not loaded: "
                  ++ show constructor
                Just binding -> functionPenalty binding @?= expectedPenalty)
            [ (TupleCon 0, Penalty 9.9)
            , (Cons, Penalty 5.0)
            , (TupleCon 2, Penalty 5.0)
            , (TupleCon 3, Penalty 5.0)
            , (TupleCon 4, Penalty 4.0)
            , (TupleCon 5, Penalty 3.0)
            , (TupleCon 6, Penalty 2.0)
            ]
          assertBool "the shipped class table is empty"
            (not $ Map.null $ sClassEnv_tclasses classEnvironment)
          assertBool "the shipped instance index is empty"
            (not $ Map.null $ sClassEnv_instances classEnvironment)
          Map.size (sClassEnv_tclasses classEnvironment) @?= 41
          sum (map length $ Map.elems $ sClassEnv_instances classEnvironment)
            @?= 535
          messages @?=
              [ "got 41 classes"
              , "and 432 instances"
            , "(-> 535 instances after inflation)"
            , "and 156 function decls"
            ]
          (goal, _) <- expectRight $ parseTypePure "()"
          case findOneExpression identityInput
              { input_goalType = goal
              , input_envFuncs = unitBindings
              } of
            Just (ExpName (TupleCon 0), _, _) -> pure ()
            Nothing -> fail "built-in unit did not inhabit ()"
            Just _ -> fail "unit search returned a different expression"
      , testCase "unboxed tuple syntax is rejected during elaboration" $ do
          let mode = enableUnboxedTuples $ haskellSrcExtsParseMode "unboxed"
          mapM_ (expectUnsupportedUnboxed . parseTypeWithModePure mode)
            [ "(# Int, Bool #)", "(# Int #)", "(#,#)" ]
      , testCase "invalid boxed tuple constructor arity is rejected" $
          mapM_ (\arity ->
              case convertQName Nothing [] $ HSE.Special HSE.noSrcSpan
                  (HSE.TupleCon HSE.noSrcSpan HSE.Boxed arity) of
                Left message -> assertBool message
                  $ ("arity " ++ show arity) `isInfixOf` message
                Right result -> fail $ "invalid tuple was accepted as " ++ show result)
            [-1, 0, 1]
      , testCase "unboxed special constructors are rejected explicitly" $ do
          let constructors =
                [ HSE.TupleCon HSE.noSrcSpan HSE.Unboxed 2
                , HSE.UnboxedSingleCon HSE.noSrcSpan
                ]
          mapM_ (\constructor ->
              case convertQName Nothing []
                  (HSE.Special HSE.noSrcSpan constructor) of
                Left message -> assertBool message
                  $ "unboxed" `isInfixOf` message
                Right result -> fail $ "unboxed constructor was accepted as "
                  ++ show result)
            constructors
      , testCase "type parse failures carry source positions" $
          case parseTypePure "(" of
            Left Diagnostic
                { diagnosticSeverity = Error
                , diagnosticSource = Just "test.hs"
                , diagnosticSpan = Just
                    (SourceSpan (SourcePosition line column) _)
                } -> (line > 0 && column > 0) @?= True
            Left result -> fail $ "incomplete diagnostic: " ++ show result
            Right result -> fail $ "malformed type was accepted: " ++ show result
      , testCase "diagnostics use the shared code and context model" $
          renderDiagnostic
            (withContext "while parsing an Exference query"
              $ withCode "EXF001"
              $ diagnostic "invalid type")
            @?= "error [EXF001]: invalid type\n\
                \  context: while parsing an Exference query"
      , testCase "current HSE parses and elaborates constrained types" $
          case parseTypePure "Eq a => a -> a" of
            Left result -> fail $ show result
            Right (TypeForall [] [HsConstraint className [TypeVar 0]]
                    (TypeArrow (TypeVar 0) (TypeVar 0)), _) ->
              className @?= name "Eq"
            Right result -> fail $ "unexpected elaboration: " ++ show result
      ]
  , testGroup "shared generated output"
      [ testCase "typed expressions erase to stable local identities" $ do
          let variable = TypeVar 0
              expression = ExpLambda 1 variable
                $ ExpApply (ExpName $ name "id") (ExpVar 1 variable)
          toGeneratedExpression expression @?=
            Generated.Lambda [Generated.Bind 1]
              (Generated.Apply
                (Generated.Global $ toSynthesisName $ name "id")
                (Generated.Local 1))
      , testCase "scope failures are reported before rendering" $
          let partial = ExpVar 7 $ TypeVar 0
          in do
            renderExpression Generated.Unqualified partial
              @?= Left (ExpressionScopeError $ Generated.UnboundLocal 7)
            showExpression partial @?= "g"
      , testCase "qualification follows the shared policy" $ do
          global <- expectRight $ mkQualifiedName ["Data", "List"] "map"
          renderExpression Generated.Unqualified (ExpName global)
            @?= Right "map"
          renderExpression Generated.FullyQualified (ExpName global)
            @?= Right "Data.List.map"
      , testCase "search batches expose the same generated tree" $ do
          let variable = TypeVar 0
              expression = ExpLambda 1 variable (ExpVar 1 variable)
              candidate = (expression, [], ExferenceStats 1 (Penalty 0) 0)
              chunk = ExferenceChunkElement
                (SearchStatus SearchExhausted 0 0) Map.empty [candidate]
          batch <- expectRight $ toGeneratedSearchBatch chunk
          case SharedSearch.batchCandidates batch of
            [(generated, [], _)] -> generated @?=
              Generated.Lambda [Generated.Bind 1] (Generated.Local 1)
            candidates -> fail $ "unexpected generated batch: "
              ++ show (length candidates)
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
      , testCase "qualified lowercase names retain Var and qualification" $
          case convert 2 (ExpName $ QualifiedName ["Data", "List"] "map") of
            HSE.Var _ (HSE.Qual _ (HSE.ModuleName _ "Data.List")
                (HSE.Ident _ "map")) -> pure ()
            expression -> fail $ "unexpected qualified value: " ++ show expression
      , testCase "qualified operators use Symbol names" $
          case convert 2 (ExpName
              $ QualifiedName ["Control", "Applicative"] "<*>") of
            HSE.Var _ (HSE.Qual _ (HSE.ModuleName _ "Control.Applicative")
                (HSE.Symbol _ "<*>")) -> pure ()
            expression -> fail $ "unexpected qualified operator: " ++ show expression
      , testCase "Unicode operators remain symbols with either qualification" $
          case ( convert 0 (ExpName $ QualifiedName [] "⊕")
               , convert 2 (ExpName $ QualifiedName ["Math", "Operators"] "⊕")
               ) of
            ( HSE.Var _ (HSE.UnQual _ (HSE.Symbol _ "⊕"))
              , HSE.Var _ (HSE.Qual _ (HSE.ModuleName _ "Math.Operators")
                  (HSE.Symbol _ "⊕"))
              ) -> pure ()
            expressions -> fail $ "unexpected Unicode operators: "
              ++ show expressions
      , testCase "symbolic constructors use Symbol in patterns" $
          let expression = convert 0 $ ExpCaseMatch
                (ExpVar 0 (TypeCons $ name "T"))
                [(name ":+:", [(1, TypeVar 0), (2, TypeVar 1)],
                  ExpVar 1 (TypeVar 0))]
          in case expression of
              HSE.Case _ _ [HSE.Alt _
                  (HSE.PApp _ (HSE.UnQual _ (HSE.Symbol _ ":+:")) [_, _]) _ _] ->
                case HSE.parseExp (HSE.prettyPrint expression) of
                  HSE.ParseOk _ -> pure ()
                  failure -> fail $ "rendered pattern does not parse: " ++ show failure
              _ -> fail $ "unexpected constructor pattern: " ++ show expression
      , testCase "symbolic type constructors use a legal binder fallback" $ do
          symbolic <- expectRight $ mkQualifiedName [] ":+:"
          case convert 0 $ ExpLambda 1 (TypeCons symbolic)
              (ExpVar 1 $ TypeCons symbolic) of
            HSE.Lambda _ [HSE.PVar _ (HSE.Ident _ binder)] _ -> do
              binder @?= "a"
              case HSE.parseExp $ "\\" ++ binder ++ " -> " ++ binder of
                HSE.ParseOk _ -> pure ()
                failure -> fail $ "invalid generated binder: " ++ show failure
            expression -> fail $ "unexpected lambda: " ++ show expression
      , testCase "binders cannot capture globals made unqualified" $ do
          global <- expectRight $ mkQualifiedName ["M"] "a"
          unqualifiedGlobal <- expectRight $ mkQualifiedName [] "a"
          integer <- expectRight $ mkQualifiedName [] "Int"
          boolean <- expectRight $ mkQualifiedName [] "Bool"
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let expression = ExpLambda 1 (TypeVar 10) (ExpName global)
              functions = [FunctionBinding (TypeCons boolean) global 0 [] []]
              classes = mkQueryClassEnv staticClasses []
          checkExpression classes functions []
            (TypeArrow (TypeCons integer) (TypeCons boolean)) [] expression
            @?= Right ()
          case convert 0 expression of
            HSE.Lambda _ [HSE.PVar _ (HSE.Ident _ binder)]
                (HSE.Var _ (HSE.UnQual _ (HSE.Ident _ occurrence))) -> do
              binder @?= "a'"
              occurrence @?= "a"
            rendered -> fail $ "capturing unqualified render: " ++ show rendered
          case convert 2 expression of
            HSE.Lambda _ [HSE.PVar _ (HSE.Ident _ binder)]
                (HSE.Var _ (HSE.Qual _ (HSE.ModuleName _ modul)
                  (HSE.Ident _ occurrence))) -> do
              binder @?= "a"
              modul @?= "M"
              occurrence @?= "a"
            rendered -> fail $ "unexpected qualified render: " ++ show rendered
          showExpression (ExpLambda 1 (TypeVar 10) $ ExpName unqualifiedGlobal)
            @?= "\\a' -> a"
      , testCase "distinct binder IDs receive distinct spellings" $ do
          typeName <- expectRight $ mkQualifiedName [] "T"
          let expression = ExpLambda 6 (TypeCons typeName)
                $ ExpLambda 33 (TypeVar 100)
                $ ExpVar 6 (TypeCons typeName)
          case convert 0 expression of
            HSE.Lambda _
                [ HSE.PVar _ (HSE.Ident _ outer)
                , HSE.PVar _ (HSE.Ident _ inner)
                ]
                (HSE.Var _ (HSE.UnQual _ (HSE.Ident _ body))) -> do
              outer @?= "t6"
              inner @?= "t6'"
              body @?= outer
              assertBool "text renderer reused a captured binder"
                $ "t6'" `isInfixOf` showExpression expression
            rendered -> fail $ "colliding binder render: " ++ show rendered
      , testCase "function conversion reserves its declaration name" $ do
          let expression = ExpLambda 1 (TypeVar 0) (ExpVar 1 $ TypeVar 0)
          case convertToFunc 0 "a" expression of
            HSE.FunBind _
                [HSE.Match _ (HSE.Ident _ function)
                  [HSE.PVar _ (HSE.Ident _ parameter)]
                  (HSE.UnGuardedRhs _
                    (HSE.Var _ (HSE.UnQual _ (HSE.Ident _ body)))) Nothing] -> do
                function @?= "a"
                parameter @?= "a'"
                body @?= parameter
            rendered -> fail $ "capturing function render: " ++ show rendered
      ]
  , testGroup "independent expression checking"
      [ testCase "environment-free simplification introduces no globals" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let variable = TypeConstant 0
              identity = ExpLambda 1 variable (ExpVar 1 variable)
              composeLike = ExpLambda 1 variable
                $ ExpApply (ExpName $ name "f")
                $ ExpApply (ExpName $ name "g") (ExpVar 1 variable)
              classEnvironment = mkQueryClassEnv staticClasses []
          assertBool "identity simplification introduced a global"
            $ simplifyExpression identity == identity
          assertBool "composition simplification introduced a global"
            $ simplifyExpression composeLike == composeLike
          checkExpression classEnvironment [] []
            (TypeArrow variable variable) [] (simplifyExpression identity)
            @?= Right ()
      , testCase "accepts a typed identity" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let variable = TypeVar 0
              identity = ExpLambda 1 variable (ExpVar 1 variable)
              classEnvironment = mkQueryClassEnv staticClasses []
          checkExpression classEnvironment [] []
            (TypeArrow variable variable) [] identity @?= Right ()
      , testCase "rejects a mismatched variable annotation" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let integer = TypeCons $ name "Int"
              boolean = TypeCons $ name "Bool"
              malformed = ExpLambda 1 integer (ExpVar 1 boolean)
              classEnvironment = mkQueryClassEnv staticClasses []
          checkExpression classEnvironment [] []
            (TypeArrow integer integer) [] malformed
            @?= Left (TypeMismatch integer boolean)
      , testCase "guards the live search result boundary" $ do
          let variable = TypeVar 0
              input = ExferenceInput
                (TypeArrow variable variable)
                [] [] emptyClassEnv
                False False 0 False 20 Nothing Nothing defaultHeuristicsConfig
          case findOneExpression input of
            Nothing -> fail "identity search produced no checked result"
            Just (expression, constraints, _) -> do
              assertBool "search returned a pre-simplification expression"
                $ expression == simplifyExpression expression
              checkExpression
                (mkQueryClassEnv emptyClassEnv []) [] []
                (TypeArrow variable variable) constraints expression
                @?= Right ()
      ]
  ]

name :: String -> QualifiedName
name = QualifiedName []

-- A deliberately tiny but complete frontend inventory.  Keeping the
-- constructor functions beside their structural declarations makes it easy
-- for the source-boundary tests above to perturb exactly one side of the
-- required bijection.
maybeLikeSourceEnvironment :: SourceEnvironment FunctionBinding
maybeLikeSourceEnvironment = SourceEnvironment
  { sourceFunctions =
      [ FunctionBinding maybeType nothingName (Penalty 1.25) [] []
      , FunctionBinding maybeType justName (Penalty 2.75) [] [TypeVar 0]
      , FunctionBinding maybeType (name "defaultMaybe")
          (Penalty 3.5) [] []
      ]
  , sourceDeconstructors =
      [ DeconstructorBinding maybeType
          [ ConstructorBinding nothingName []
          , ConstructorBinding justName [TypeVar 0]
          ]
          False
      ]
  , sourceClasses = emptyStaticClassEnv
  , sourceTypeNames = [maybeName]
  , sourceTypeSynonyms = []
  }
 where
  maybeName = name "Maybe"
  nothingName = name "Nothing"
  justName = name "Just"
  maybeType = TypeApp (TypeCons maybeName) (TypeVar 0)

assertUnifierCloses :: HsType -> HsType -> IO ()
assertUnifierCloses left right = case unify left right of
  Nothing -> pure ()
  Just (leftSubstitutions, rightSubstitutions) -> do
    let leftResult = snd $ applySubsts leftSubstitutions left
        rightResult = snd $ applySubsts rightSubstitutions right
    assertBool
      ( "unclosed symmetric substitution for " ++ show left ++ " ~ " ++ show right
        ++ ": " ++ show leftResult ++ " /= " ++ show rightResult
        ++ " from " ++ show leftSubstitutions
        ++ " and " ++ show rightSubstitutions
      )
      (leftResult == rightResult)

assertOffsetUnifierCloses :: Int -> HsType -> HsType -> IO ()
assertOffsetUnifierCloses offset left right =
  case unifyOffset left (HsTypeOffset right offset) of
    Nothing -> pure ()
    Just (leftSubstitutions, rightSubstitutions) -> do
      let shiftedRight = incVarIds (+ offset) right
          leftResult = snd $ applySubsts leftSubstitutions left
          rightResult = snd $ applySubsts rightSubstitutions shiftedRight
      assertBool
        ( "unclosed offset substitution for " ++ show left ++ " ~ " ++ show right
          ++ ": " ++ show leftResult ++ " /= " ++ show rightResult
          ++ " from " ++ show leftSubstitutions
          ++ " and " ++ show rightSubstitutions
        )
        (leftResult == rightResult)

parseTypePure :: String -> Either Diagnostic (HsType, TypeVarIndex)
parseTypePure = parseTypeWithModePure $ haskellSrcExtsParseMode "test"

isLoaderSummary :: String -> Bool
isLoaderSummary message = any (`isPrefixOf` message)
  ["got ", "and ", "(-> "]

parseTypeWithModePure
  :: HSE.ParseMode
  -> String
  -> Either Diagnostic (HsType, TypeVarIndex)
parseTypeWithModePure mode source = runIdentity
  $ runMultiRWSTNil
  $ runExceptT
  $ parseType Map.empty Nothing [] Map.empty mode source

enableUnboxedTuples :: HSE.ParseMode -> HSE.ParseMode
enableUnboxedTuples mode = mode
  { HSE.extensions = HSE.EnableExtension HSE.UnboxedTuples
      : HSE.extensions mode
  }

expectUnsupportedUnboxed
  :: Either Diagnostic (HsType, TypeVarIndex)
  -> IO ()
expectUnsupportedUnboxed result = case result of
  Left Diagnostic
      { diagnosticSource = Just _
      , diagnosticSpan = Just _
      , diagnosticMessage = message
      } -> assertBool message $ "unboxed" `isInfixOf` message
  Left diagnosticResult -> fail $ "incomplete diagnostic: " ++ show diagnosticResult
  Right elaborated -> fail $ "unboxed syntax was accepted as " ++ show elaborated

loadEnvironmentAndMessages
  :: FilePath
  -> IO ([FunctionBinding], StaticClassEnv, [String])
loadEnvironmentAndMessages environmentDirectory = do
  ((bindings, classEnvironment), messages) <- runMultiRWSTNil
    $ withMultiWriterAW
    $ do
        environmentResult <- environmentFromPath environmentDirectory
        checkedEnvironment <- either (lift . fail . show) pure environmentResult
        let environment = checkedSourceProjection checkedEnvironment
        pure (sourceFunctions environment, sourceClasses environment)
  pure (bindings, classEnvironment, messages)

identityInput :: ExferenceInput
identityInput = ExferenceInput
  (TypeArrow (TypeVar 0) (TypeVar 0))
  [] [] emptyClassEnv
  False False 0 False 20 Nothing Nothing defaultHeuristicsConfig

onlyChunk :: ExferenceInput -> IO ExferenceChunkElement
onlyChunk input = case findExpressionsWithStats input of
  [chunk] -> pure chunk
  chunks -> fail $ "expected one search chunk, got " ++ show (length chunks)

lastChunk :: ExferenceInput -> IO ExferenceChunkElement
lastChunk input = case findExpressionsWithStats input of
  [] -> fail "expected at least one search chunk"
  chunk : chunks -> pure $ go chunk chunks
 where
  go latest [] = latest
  go _ (next : rest) = go next rest

expectRight :: Show problem => Either problem result -> IO result
expectRight = either (fail . show) pure

assertNameRejected
  :: Either QualifiedNameError QualifiedName
  -> IO ()
assertNameRejected (Left _) = pure ()
assertNameRejected (Right result) =
  fail $ "invalid name was accepted as " ++ show result

expectParsedModule :: String -> IO (HSE.Module HSE.SrcSpanInfo)
expectParsedModule source = case HSE.parseModuleWithMode
    (haskellSrcExtsParseMode "qualified-class-test") source of
  HSE.ParseOk modul -> pure modul
  failure -> fail $ "module did not parse: " ++ show failure

withTemporaryFile :: String -> (FilePath -> IO a) -> IO a
withTemporaryFile source action = do
  temporaryDirectory <- getTemporaryDirectory
  bracket
    (do
      (path, handle) <- openTempFile temporaryDirectory
        "exference-loader-module.hs"
      hPutStr handle source
      hClose handle
      pure path)
    removeFile
    action

classEnvironmentFromSources
  :: [String]
  -> IO (Either ClassEnvironmentLoadError StaticClassEnv)
classEnvironmentFromSources sources = do
  modules <- mapM expectParsedModule sources
  let result = runIdentity $ runMultiRWSTNil $
        getClassEnv [] Map.empty modules
  pure $ fst <$> result
