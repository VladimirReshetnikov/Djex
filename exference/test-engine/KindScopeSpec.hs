module KindScopeSpec (tests) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

import Language.Haskell.Exference.Core.Internal.FlexibleIds (allocateNamespace)
import Language.Haskell.Exference.Core.Expression (Expression (..))
import qualified Language.Haskell.Exference.Core.Internal.ExpressionCheck as Check
import Language.Haskell.Exference.Core.FunctionBinding (EnvDictionary (..))
import Language.Haskell.Exference.Core.Internal.KindScope
import Language.Haskell.Exference.Core.Internal.Polytype
  ( instantiateLeadingForallsWithOpenings, leadingForallOpeningBindings )
import Language.Haskell.Exference.Core.Internal.VariableSupply (supplyFromIdentifiers)
import Language.Haskell.Exference.Core.Types
  ( HsType, SynthesisVariable, emptyStaticClassEnv, mkQueryClassEnv )
import Language.Haskell.Exference.Core.RigidInstantiation
  ( planRigidInstantiation, mkRigidInstantiationContext )
import qualified Language.Haskell.Synthesis.Generated as G
import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Kind (Kind (..))
import Language.Haskell.Synthesis.KindInference
  ( GroundKind, KindAssumptions (..), emptyKindAssumptions )
import Language.Haskell.Synthesis.Name (Name, Boxity (Boxed), mkIdentifier, functionName)
import qualified Language.Haskell.Synthesis.SourceKind as S
import qualified Language.Haskell.Synthesis.Type as T

tests :: TestTree
tests = testGroup "lexical kind scope"
  [ testCase "shadowed binders acquire separate kinds outside the environment namespace" $ do
      let source = T.ForallType [v 0] [Constraint c [ty 0]] $
            T.ForallType [v 0] [] $ T.TypeApplication (ty 0) unit
          outerAssumptions = emptyKindAssumptions
            { classParameterKinds = Map.singleton c [Just proper] }
      (prepared, scope) <- prepare outerAssumptions source
        [S.SourceKindAnnotation [S.ForallBody] 0 higher]
      case prepared of
        T.ForallType [outer] [Constraint _ [T.TypeVariable argument]]
            (T.ForallType [inner] [] body) -> do
          argument @?= outer
          body @?= T.TypeApplication (T.TypeVariable inner) unit
          assertBool "reserved identities escaped" $
            all (\variable -> Set.notMember variable reserved) [outer, inner]
          assertBool "shadowing collapsed" $ outer /= inner
          kindScopeBinderKind scope outer @?= Just proper
          kindScopeBinderKind scope inner @?= Just higher
        _ -> assertFailure $ show prepared
  , testCase "actual openings retain vacuous kinds and empty contextual layers" $ do
      let source = T.ForallType [] [] $
            T.ForallType [v 0] [] $ T.ForallType [v 0] [] unit
      (prepared, scope) <- prepare emptyKindAssumptions source
        [S.SourceKindAnnotation [S.ForallBody] 0 higher]
      (body, constraints, openings, _) <- maybe (fail "allocation failed") pure $
        instantiateLeadingForallsWithOpenings allocateNamespace
          (supplyFromIdentifiers [0..40]) prepared
      body @?= unit
      constraints @?= []
      map (length . leadingForallOpeningBindings) openings @?= [0,1,1]
      opened <- right $ retainLeadingForallKinds scope openings
      let allocated = concatMap (map (v . snd) . leadingForallOpeningBindings) openings
      map (kindScopeBinderKind opened) allocated @?= [Just higher, Just proper]
      forM_ allocated $ \variable ->
        assertBool "allocation reused a source identity" $
          Set.notMember variable $ kindScopeVariables scope
  , testCase "vacuous higher binder rejects a proper-type instantiation" $ do
      (prepared, scope) <- higherSource
      binder <- onlyBinder prepared
      left $ substituteKindScope scope (Map.singleton binder unit) []
  , testCase "substitution chains preserve higher kinds and reject a later mismatch" $ do
      (prepared, scope) <- higherSource
      binder <- onlyBinder prepared
      (_, pending) <- right $ substituteKindScope scope
        (Map.singleton binder $ ty 100) []
      kindScopeBinderKind pending (v 100) @?= Just higher
      left $ substituteKindScope pending (Map.singleton (v 100) unit) []
      (_, valid) <- right $ substituteKindScope pending
        (Map.singleton (v 100) list) []
      checkKindScopeTypes valid [] @?= Right ()
  , testCase "intermediate application arguments are not defaulted prematurely" $ do
      (prepared, scope) <- prepare assumptions
        (T.ForallType [v 0] [] unit) []
      binder <- onlyBinder prepared
      (_, pending) <- right $ substituteKindScope scope
        (Map.singleton binder $ T.TypeApplication (ty 100) $ ty 101) []
      kindScopeBinderKind pending (v 100) @?= Nothing
      kindScopeBinderKind pending (v 101) @?= Nothing
      -- Either a constructor taking proper types or one taking constructors
      -- is legal. Solving the first obligation must not discard either.
      (_, higherArgument) <- right $ substituteKindScope pending
        (Map.fromList [(v 100, T.TypeConstructor h), (v 101, list)]) []
      checkKindScopeTypes higherArgument [] @?= Right ()
      (_, properArgument) <- right $ substituteKindScope pending
        (Map.fromList [(v 100, list), (v 101, unit)]) []
      checkKindScopeTypes properArgument [] @?= Right ()
      left $ substituteKindScope pending
        (Map.fromList [(v 100, list), (v 101, list)]) []
  , testCase "capture avoidance carries the renamed binder's kind" $ do
      (prepared, scope) <- prepare assumptions
        (T.ForallType [v 0] [] $ ty 8)
        [S.SourceKindAnnotation [] 0 higher]
      binder <- onlyBinder prepared
      let image = T.TypeApplication (T.TypeVariable binder) unit
      (results, updated) <- right $ substituteKindScope scope
        (Map.singleton (v 8) image) [prepared]
      case results of
        [result@(T.ForallType [renamed] [] body)] -> do
          assertBool "replacement was captured" $ renamed /= binder
          body @?= image
          kindScopeBinderKind updated renamed @?= Just higher
          kindScopeBinderKind updated binder @?= Just higher
          checkKindScopeTypes updated [(proper, result)] @?= Right ()
        _ -> assertFailure $ show results
  , testCase "simultaneous substitutions do not recursively rewrite their images" $ do
      (prepared, scope) <- higherSource
      binder <- onlyBinder prepared
      (results, updated) <- right $ substituteKindScope scope
        (Map.fromList [(binder, ty 100), (v 100, unit)]) [T.TypeVariable binder]
      results @?= [ty 100]
      -- The old free 100 maps to unit, but binder's simultaneous image is
      -- the new free 100. It keeps the binder's higher kind.
      kindScopeBinderKind updated (v 100) @?= Just higher
      checkKindScopeTypes updated [(higher, ty 100)] @?= Right ()
  , testCase "opaque forall equality checks vacuous kinds under callbacks" $ do
      let quantified = T.ForallType [v 0] [] unit
          callback = T.FunctionType quantified unit
          source = T.TupleType Boxed [callback, callback, callback]
      (prepared, scope) <- prepare assumptions source
        [ S.SourceKindAnnotation [S.TupleElement 0, S.FunctionParameter] 0 higher
        , S.SourceKindAnnotation [S.TupleElement 2, S.FunctionParameter] 0 higher ]
      case prepared of
        T.TupleType _ [a,b,d] -> do
          checkKindScopeTypes scope [(proper, a), (proper, b)] @?= Right ()
          left $ checkKindScopeCompatibility scope proper a b
          checkKindScopeCompatibility scope proper a d @?= Right ()
          case b of
            T.FunctionType parameter result ->
              left $ checkKindScopeCompatibility scope proper a $
                T.TypeApplication
                  (T.TypeApplication (T.TypeConstructor functionName) parameter) result
            _ -> assertFailure $ show b
        _ -> assertFailure $ show prepared
  , testCase "provider schemes retain independent vacuous binder kinds" $ do
      let source = T.ForallType [v 0] [] unit
      (firstType, scope) <- prepare assumptions source
        [S.SourceKindAnnotation [] 0 higher]
      checked <- right $ S.prepareSourceTypeKinds assumptions source []
      (secondType, extended) <- right $ extendKindScope reserved scope checked
      firstBinder <- onlyBinder firstType
      secondBinder <- onlyBinder secondType
      assertBool "provider source IDs collided" $ firstBinder /= secondBinder
      kindScopeBinderKind extended firstBinder @?= Just higher
      kindScopeBinderKind extended secondBinder @?= Just proper
      left $ checkKindScopeCompatibility extended proper firstType secondType
  , testCase "a source from another nominal inventory cannot extend the scope" $ do
      (_, scope) <- higherSource
      checked <- right $ S.prepareSourceTypeKinds emptyKindAssumptions unit []
      left $ extendKindScope reserved scope checked
  , testCase "forall allocation cannot borrow an existing provider identity of the same kind" $ do
      (firstType, scope) <- higherSource
      checked <- right $ S.prepareSourceTypeKinds assumptions
        (T.ForallType [v 0] [] unit) [S.SourceKindAnnotation [] 0 higher]
      (otherType, extended) <- right $ extendKindScope
        (Set.fromList $ map v [0..20]) scope checked
      otherBinder <- onlyBinder otherType
      -- This deliberately incomplete supply reserves the first scheme,
      -- but omits the newly acquired provider. Reject its colliding result.
      (_, _, openings, _) <- maybe (fail "allocation failed") pure $
        instantiateLeadingForallsWithOpenings allocateNamespace
          (supplyFromIdentifiers []) firstType
      concatMap (map (v . snd) . leadingForallOpeningBindings) openings @?= [otherBinder]
      left $ retainLeadingForallKinds extended openings
  , testCase "forall body formation and class arguments survive lexical opening" $ do
      let classAssumptions = assumptions
            { classParameterKinds = Map.singleton c [Just higher] }
      (prepared, scope) <- prepare classAssumptions
        (T.ForallType [v 0] [Constraint c [ty 0]] unit) []
      binder <- onlyBinder prepared
      kindScopeBinderKind scope binder @?= Just higher
      checkKindScopeTypes scope [(proper, prepared)] @?= Right ()
      left $ checkKindScopeTypes scope
        [(proper, T.ForallType [] [] list)]
      left $ checkKindScopeTypes scope
        [(proper, T.ForallType [] [Constraint c [unit]] unit)]
      checkKindScopeEquality scope list list @?= Right ()
      left $ checkKindScopeEquality scope list unit
      left $ checkKindScopeEquality scope list $ T.TypeConstructor h
  , testCase "malformed selected binders are rejected before lexical erasure" $ do
      (_, scope) <- higherSource
      left $ checkKindScopeTypes scope
        [(proper, T.ForallType [v 100, v 100] [] unit)]
  , testCase "independent checker rejects wrong-kind visible selection of a vacuous binder" $ do
      let provider = T.ForallType [v 0] [] unit
      (goal, scope) <- prepare assumptions (T.FunctionType provider unit)
        [S.SourceKindAnnotation [S.FunctionParameter] 0 higher]
      context <- checker scope goal
      case goal of
        T.FunctionType scheme _ -> do
          wrong <- right $ G.specifiedVisibleTypeArgument unit
          correct <- right $ G.specifiedVisibleTypeArgument list
          let expression argument = ExpLambda 0 scheme $
                ExpTypeApply (ExpVar 0 scheme) argument
          Check.checkExpression (mkQueryClassEnv emptyStaticClassEnv []) [] []
            goal [] (expression wrong) @?= Right ()
          kindRejection $ Check.checkExpressionInContext context [] $ expression wrong
          Check.checkExpressionInContext context [] (expression correct) @?= Right ()
        _ -> assertFailure $ show goal
  , testCase "independent checker rejects a kind-erased polymorphic lambda annotation" $ do
      let provider = T.ForallType [v 0] [] unit
      (goal, scope) <- prepare assumptions
        (T.FunctionType provider $ T.FunctionType provider unit)
        [S.SourceKindAnnotation [S.FunctionParameter] 0 higher]
      context <- checker scope goal
      case goal of
        T.FunctionType higherProvider (T.FunctionType properProvider _) -> do
          let expression annotation = ExpLambda 0 annotation $
                ExpLambda 1 properProvider $ ExpTuple []
          Check.checkExpression (mkQueryClassEnv emptyStaticClassEnv []) [] []
            goal [] (expression properProvider) @?= Right ()
          kindRejection $ Check.checkExpressionInContext context [] $
            expression properProvider
          Check.checkExpressionInContext context [] (expression higherProvider) @?= Right ()
        _ -> assertFailure $ show goal
  , testCase "independent checker retains root rigid and implicit local kinds" $ do
      let provider = T.ForallType [v 1] [] unit
      (goal, scope) <- prepare assumptions
        (T.ForallType [v 0] [] $ T.FunctionType provider unit)
        [ S.SourceKindAnnotation [] 0 higher
        , S.SourceKindAnnotation [S.ForallBody, S.FunctionParameter] 0 higher ]
      context <- checker scope goal
      case goal of
        T.ForallType _ _ (T.FunctionType scheme _) ->
          Check.checkExpressionInContext context []
            (ExpLambda 0 scheme $ ExpVar 0 unit) @?= Right ()
        _ -> assertFailure $ show goal
  , testCase "independent shallow subsumption checks nested vacuous kinds" $ do
      let provider = T.ForallType [v 0] [] $ T.FunctionType (ty 0) (ty 0)
          argument = T.ForallType [v 1] [] unit
          result = T.ForallType [v 2] [] unit
          target = T.ForallType [v 3] [] $ T.FunctionType argument result
          higherAt step = S.SourceKindAnnotation
            [S.FunctionResult, S.ForallBody, step] 0 higher
      forM_ [False, True] $ \matching -> do
        (goal, scope) <- prepare assumptions (T.FunctionType provider target) $
          higherAt S.FunctionParameter :
            [higherAt S.FunctionResult | matching]
        context <- checker scope goal
        case goal of
          T.FunctionType source requested -> do
            let expression = ExpLambda 0 source $ ExpVar 0 requested
            Check.checkExpression (mkQueryClassEnv emptyStaticClassEnv []) [] []
              goal [] expression @?= Right ()
            if matching
              then Check.checkExpressionInContext context [] expression @?= Right ()
              else kindRejection $ Check.checkExpressionInContext context [] expression
          _ -> assertFailure $ show goal
  , testCase "a checker context cannot omit kinds for an unused source callback" $ do
      (_, unrelated) <- prepare assumptions unit []
      let goal = T.FunctionType (T.ForallType [v 100] [] unit) unit
      plan <- right $ planRigidInstantiation
        (mkRigidInstantiationContext $ EnvDictionary [] [] emptyStaticClassEnv) [] goal
      case Check.prepareExpressionCheckContextWithKindScope unrelated plan
          (mkQueryClassEnv emptyStaticClassEnv []) [] [] Map.empty goal of
        Left (Check.InvalidCheckKind _) -> pure ()
        Left failure -> assertFailure $ "wrong rejection: " ++ show failure
        Right _ -> assertFailure "unowned source callback acquired kind authority"
  ]

proper, higher :: GroundKind
proper = ProperTypeKind
higher = FunctionKind proper proper

c, h, listName :: Name
c = named "C"
h = named "H"
listName = named "List"

assumptions :: KindAssumptions
assumptions = emptyKindAssumptions
  { typeConstructorKinds = Map.fromList
      [(listName, higher), (h, FunctionKind higher proper)] }

v :: Int -> SynthesisVariable
v = T.FlexibleVariable

ty :: Int -> HsType
ty = T.TypeVariable . v

unit, list :: HsType
unit = T.TupleType Boxed []
list = T.TypeConstructor listName

reserved :: Set.Set SynthesisVariable
reserved = Set.fromList $ map v [0..9]

prepare
  :: KindAssumptions -> HsType -> [S.SourceKindAnnotation]
  -> IO (HsType, KindScope)
prepare kinds source annotations = do
  checked <- right $ S.prepareSourceTypeKinds kinds source annotations
  right $ prepareKindScope reserved checked

higherSource :: IO (HsType, KindScope)
higherSource = prepare assumptions (T.ForallType [v 0] [] unit)
  [S.SourceKindAnnotation [] 0 higher]

onlyBinder :: HsType -> IO SynthesisVariable
onlyBinder (T.ForallType [binder] _ _) = pure binder
onlyBinder source = fail $ show source

right :: Show failure => Either failure value -> IO value
right = either (fail . show) pure

left :: Show value => Either failure value -> IO ()
left (Left _) = pure ()
left (Right value) = assertFailure $ "unexpected acceptance: " ++ show value

named :: String -> Name
named = either (error . show) id . mkIdentifier

checker :: KindScope -> HsType -> IO Check.ExpressionCheckContext
checker scope goal = do
  plan <- right $ planRigidInstantiation
    (mkRigidInstantiationContext $ EnvDictionary [] [] emptyStaticClassEnv) [] goal
  right $ Check.prepareExpressionCheckContextWithKindScope scope plan
    (mkQueryClassEnv emptyStaticClassEnv []) [] [] Map.empty goal

kindRejection :: Either Check.ExpressionCheckError () -> IO ()
kindRejection (Left (Check.InvalidCheckKind _)) = pure ()
kindRejection result = assertFailure $ "expected a kind-specific rejection: " ++ show result
