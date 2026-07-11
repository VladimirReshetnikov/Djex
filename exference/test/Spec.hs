module Main (main) where

import Data.Monoid (Any (..))
import Data.Bifunctor (first)
import Data.Functor.Identity (runIdentity)
import Data.List (find, isInfixOf)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)
import Control.Monad.Trans.Except (runExceptT)
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
  , constraintsRelaxedAtStep
  , findExpressionsWithStats
  )
import Language.Haskell.Exference.Core.ConstraintSolver
import Language.Haskell.Exference.Core.Expression (Expression (..))
import Language.Haskell.Exference.Core.ExpressionCheck
import Language.Haskell.Exference.Core.FunctionBinding (FunctionBinding (..))
import qualified Language.Haskell.Exference.Core.Internal.Scope as Scope
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.Unify
import Language.Haskell.Exference
  ( ExferenceInput (..)
  , ExferenceInputError (..)
  , Penalty (..)
  , findExpressionsEither
  , findOneExpression
  )
import Language.Haskell.Exference.EnvironmentParser
  ( compileWithDict
  , environmentFromPath
  , parseRatings
  )
import Language.Haskell.Exference.Diagnostic
import Language.Haskell.Exference.ExpressionToHaskellSrc (convert)
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
  ( HsTypeDecl (..)
  , applyTypeDecls
  , parseType
  )
import Language.Haskell.Exference.TypeFromHaskellSrc
  ( haskellSrcExtsParseMode
  , parseQualifiedName
  , convertQName
  )
import Language.Haskell.Exference.SimpleDict (defaultHeuristicsConfig, emptyClassEnv)
import Paths_exference (getDataFileName)

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
                False False 0 False 20 Nothing Nothing defaultHeuristicsConfig
          case findExpressionsEither input of
            Left actual -> actual @?= NestedForallInGoal goal
            Right _ -> fail "nested forall was accepted"
      , testCase "step exhaustion is reported explicitly" $ do
          chunk <- onlyChunk $ identityInput {input_maxSteps = 1}
          searchCompletion (chunkStatus chunk) @?= SearchStepLimitReached
      , testCase "queue pruning is bounded and reported" $ do
          chunk <- onlyChunk $ identityInput {input_maxQueueSize = Just 0}
          searchCompletion (chunkStatus chunk) @?= SearchExhausted
          searchQueuePruned (chunkStatus chunk) @?= 1
      , testCase "depth pruning is configured and reported" $ do
          let config = defaultHeuristicsConfig
                {heuristics_functionGoalTransform = 1}
          chunk <- onlyChunk $ identityInput
            { input_maxDepth = Just 0
            , input_heuristicsConfig = config
            }
          searchCompletion (chunkStatus chunk) @?= SearchExhausted
          searchDepthPruned (chunkStatus chunk) @?= 1
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
      , testCase "qualified names reject empty path segments" $
          case parseQualifiedName "Data..map" of
            Left _ -> pure ()
            Right result -> fail $ "malformed name was accepted: " ++ show result
      , testCase "qualified operators retain module and spelling" $
          parseQualifiedName "Control.Applicative.(<*>)"
            @?= Right (QualifiedName ["Control", "Applicative"] "<*>")
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
          (bindings, messages) <- loadBindingsAndMessages environmentDirectory
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
          assertBool "structural rating lookup left an operator unmatched"
            (not $ any ("rating could not be applied: Control.Applicative.(<*>)"
              `isInfixOf`) messages)
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
                False False 0 False 20 Nothing Nothing defaultHeuristicsConfig
          case findOneExpression input of
            Nothing -> fail "identity search produced no checked result"
            Just _ -> pure ()
      ]
  ]

name :: String -> QualifiedName
name = QualifiedName []

parseTypePure :: String -> Either Diagnostic (HsType, TypeVarIndex)
parseTypePure = parseTypeWithModePure $ haskellSrcExtsParseMode "test"

parseTypeWithModePure
  :: HSE.ParseMode
  -> String
  -> Either Diagnostic (HsType, TypeVarIndex)
parseTypeWithModePure mode source = runIdentity
  $ runMultiRWSTNil
  $ runExceptT
  $ parseType [] Nothing [] Map.empty mode source

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

loadBindingsAndMessages :: FilePath -> IO ([FunctionBinding], [String])
loadBindingsAndMessages environmentDirectory = runMultiRWSTNil
  $ withMultiWriterAW
  $ do
      (bindings, _, _, _, _) <- environmentFromPath environmentDirectory
      pure bindings

identityInput :: ExferenceInput
identityInput = ExferenceInput
  (TypeArrow (TypeVar 0) (TypeVar 0))
  [] [] emptyClassEnv
  False False 0 False 20 Nothing Nothing defaultHeuristicsConfig

onlyChunk :: ExferenceInput -> IO ExferenceChunkElement
onlyChunk input = case findExpressionsWithStats input of
  [chunk] -> pure chunk
  chunks -> fail $ "expected one search chunk, got " ++ show (length chunks)

expectRight :: Show problem => Either problem result -> IO result
expectRight = either (fail . show) pure
