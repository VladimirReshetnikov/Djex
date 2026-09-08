-- Preserve constructor declaration ownership across implicit instantiation.
module ImplicitConstructorGraphSpec (tests) where

import qualified Data.Set as Set
import qualified Language.Haskell.Exference.Core.Expression as E
import Language.Haskell.Exference.Core.FunctionBinding
  ( ConstructorBinding (..), DeconstructorBinding (..), EnvDictionary (..)
  , FunctionBinding (..), functionBindingType )
import Language.Haskell.Exference.Core.Internal.ExpressionCheck
import Language.Haskell.Exference.Core.Internal.RigidScope
  (emptyRigidScope, nestedRigidProvenance)
import Language.Haskell.Exference.Core.RigidInstantiation
  (mkRigidInstantiationContext, planRigidInstantiation, rigidInstantiations)
import Language.Haskell.Exference.Core.TypeUtils (forallify)
import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.Generated as G
import qualified Language.Haskell.Synthesis.Type as T
import qualified Language.Haskell.Synthesis.TypeAtom as A
import qualified Language.Haskell.Synthesis.TypedGenerated as Q
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)

tests :: TestTree
tests = testGroup "Implicit constructor root graph ownership"
  [ testCase "zero constructor keeps its closed declaration before root instantiation" $ do
      let goal = spine $ TypeVar 1
      (rigid, expression, evidence) <- checked [zero, step] [deconstructor]
        goal $ const $ E.ExpName zeroName
      graph <- requireGraph expression evidence
      assertConstructor goal zeroName (forallify $ functionBindingType zero)
        (TypeConstant rigid) graph
  , testCase "step constructor selects the actual nested spine payload" $ do
      let inner = spine $ TypeVar 1
          outer = spine inner
          goal = TypeArrow inner $ TypeArrow outer outer
          expression rigid =
            let openedInner = spine $ TypeConstant rigid
                openedOuter = spine openedInner
            in E.ExpLambda 1 openedInner $ E.ExpLambda 2 openedOuter $
              E.ExpApply (E.ExpApply (E.ExpName stepName) $ E.ExpVar 1 openedInner)
                $ E.ExpVar 2 openedOuter
      (rigid, actual, evidence) <- checked [zero, step] [deconstructor] goal expression
      graph <- requireGraph actual evidence
      assertConstructor goal stepName (forallify $ functionBindingType step)
        (spine $ TypeConstant rigid) graph
  , testCase "monomorphic zero constructor has no vacuous source wrapper" $ do
      monomorphicGraph zeroName (functionBindingType monomorphicZero)
        monomorphicSpine $ E.ExpName zeroName
  , testCase "monomorphic step constructor remains directly applicable" $ do
      let goal = TypeArrow payload $ TypeArrow monomorphicSpine monomorphicSpine
          expression = E.ExpLambda 1 payload $ E.ExpLambda 2 monomorphicSpine $
            E.ExpApply (E.ExpApply (E.ExpName stepName) $ E.ExpVar 1 payload)
              $ E.ExpVar 2 monomorphicSpine
      monomorphicGraph stepName (functionBindingType monomorphicStep) goal expression
  , testCase "a same-spelled ordinary binding has no constructor closure authority" $ do
      (_, _, evidence) <- checked [zero] [] (spine $ TypeVar 1) $
        const $ E.ExpName zeroName
      assertGlobalScopeRejected evidence
  , testCase "a mismatched constructor result cannot supply closure authority" $ do
      let other element = TypeApp (TypeCons $ sourceName "OtherSpine") element
          mismatched = zero { functionResult = other $ TypeVar 0 }
      (_, _, evidence) <- checked [mismatched] [deconstructor]
        (other $ TypeVar 1) $ const $ E.ExpName zeroName
      assertGlobalScopeRejected evidence
  ]

spine :: HsType -> HsType
spine = TypeApp $ TypeCons $ sourceName "ImplicitSpine"

zeroName, stepName :: QualifiedName
zeroName = sourceName "ImplicitEmpty"
stepName = sourceName "ImplicitStep"

zero, step :: FunctionBinding
zero = FunctionBinding (spine $ TypeVar 0) zeroName 0 [] []
step = FunctionBinding (spine $ TypeVar 0) stepName 0 []
  [TypeVar 0, spine $ TypeVar 0]

deconstructor :: DeconstructorBinding
deconstructor = DeconstructorBinding (spine $ TypeVar 0)
  [ConstructorBinding zeroName [], ConstructorBinding stepName [TypeVar 0, spine $ TypeVar 0]] True

sourceName :: String -> QualifiedName
sourceName = either (error . show) id . mkQualifiedName []

expectRight :: Show failure => Either failure value -> IO value
expectRight = either (fail . show) pure

checked :: [FunctionBinding] -> [DeconstructorBinding] -> HsType
  -> (TVarId -> E.Expression) -> IO (TVarId, E.Expression, CheckedExpressionEvidence)
checked functions deconstructors goal makeExpression = do
  let classes = emptyStaticClassEnv
  plan <- expectRight $ planRigidInstantiation
    (mkRigidInstantiationContext $ EnvDictionary functions deconstructors classes) [] goal
  rigid <- case rigidInstantiations plan of
    [(_, selected)] -> pure selected
    actual -> fail $ "fixture requires one exact root opening: " ++ show actual
  context <- expectRight $ prepareExpressionCheckContext plan
    (mkQueryClassEnv classes []) functions deconstructors goal
  let expression = makeExpression rigid
  evidence <- expectRight $ checkExpressionInContextWithNestedRigidProvenanceEvidence
    context (nestedRigidProvenance emptyRigidScope) [] expression
  assertBool "an implicit constructor fabricated a visible source certificate" $
    null $ checkedExpressionTypeApplicationOrigins evidence
  pure (rigid, expression, evidence)

requireGraph :: E.Expression -> CheckedExpressionEvidence -> IO (Q.TermGraph HsType TVarId)
requireGraph expression evidence = case checkedExpressionTermGraph 149 evidence of
  ExferenceTermGraphAvailable graph -> do
    assertEqual "constructor graph changed exact compatibility erasure"
      (G.discardUnusedPatternBindingsBy id $ E.toGeneratedExpression expression)
      (Q.eraseTermGraph graph)
    pure graph
  actual -> fail $ "expected source-owned implicit constructor graph: " ++ show actual

assertConstructor :: HsType -> QualifiedName -> HsType -> HsType
  -> Q.TermGraph HsType TVarId -> Assertion
assertConstructor goal owner source selected graph = do
  case Q.lookupTermNode (Q.termGraphRoot graph) graph of
    Just (Q.TermNode root Q.TypedForallIntroduction{}) -> do
      assertBool "constructor lost the exact closed root" $
        A.alphaEquivalentTypes (forallify goal) root && Set.null (T.freeVariables root)
    actual -> fail $ "constructor lost its root introduction: " ++ show actual
  let globals = [(node, ty) | (node, Q.TermNode ty (Q.TypedGlobal _ name)) <- Q.termGraphNodes graph,
                  name == owner]
  (globalNode, globalType) <- case globals of
    [value] -> pure value
    actual -> fail $ "missing exact constructor global: " ++ show actual
  assertBool "constructor global contains a local root skolem or changed its declaration" $
    Set.null (T.freeVariables globalType) && A.alphaEquivalentTypes source globalType
  let applications = [witness | (_, Q.TermNode _ (Q.TypedImplicitTypeApplication _ child witness))
                                <- Q.termGraphNodes graph, child == globalNode]
  case applications of
    [witness] -> do
      assertEqual "constructor selected another root/payload type" selected $
        Q.implicitTypeApplicationSelected witness
      assertBool "constructor instantiation lost exact source/result correlation" $
        A.isLeadingForallInstantiation (Q.implicitTypeApplicationSource witness)
          selected (Q.implicitTypeApplicationResult witness)
    actual -> fail $ "constructor lacks its exact implicit application edge: " ++ show actual

assertGlobalScopeRejected :: CheckedExpressionEvidence -> Assertion
assertGlobalScopeRejected evidence = case checkedExpressionTermGraph 151 evidence of
  ExferenceTermGraphUnavailable
      (TermGraphSealingFailure Q.ForallIntroductionVariableInGlobal{}) -> pure ()
  actual -> fail $ "unowned binding acquired constructor graph authority: " ++ show actual


payload, monomorphicSpine :: HsType
payload = TypeCons $ sourceName "Payload"
monomorphicSpine = spine payload

monomorphicZero, monomorphicStep :: FunctionBinding
monomorphicZero = zero { functionResult = monomorphicSpine }
monomorphicStep = step
  { functionResult = monomorphicSpine
  , functionParameters = [payload, monomorphicSpine]
  }

monomorphicGraph :: QualifiedName -> HsType -> HsType -> E.Expression -> Assertion
monomorphicGraph owner source goal expression = do
  let functions = [monomorphicZero, monomorphicStep]
      constructors = [ConstructorBinding zeroName [],
        ConstructorBinding stepName [payload, monomorphicSpine]]
      deconstructors = [DeconstructorBinding monomorphicSpine constructors True]
      classes = emptyStaticClassEnv
  plan <- expectRight $ planRigidInstantiation
    (mkRigidInstantiationContext $ EnvDictionary functions deconstructors classes) [] goal
  assertEqual "monomorphic source acquired a root type binder" [] $ rigidInstantiations plan
  context <- expectRight $ prepareExpressionCheckContext plan
    (mkQueryClassEnv classes []) functions deconstructors goal
  evidence <- expectRight $ checkExpressionInContextWithNestedRigidProvenanceEvidence
    context (nestedRigidProvenance emptyRigidScope) [] expression
  assertBool "monomorphic constructor fabricated a visible certificate" $
    null $ checkedExpressionTypeApplicationOrigins evidence
  graph <- requireGraph expression evidence
  case Q.lookupTermNode (Q.termGraphRoot graph) graph of
    Just root -> assertEqual "monomorphic root acquired a vacuous forall" goal $ Q.termNodeType root
    Nothing -> fail "sealed monomorphic graph lost its root"
  assertEqual "constructor lost its exact unwrapped monomorphic declaration"
    [(owner, source)]
    [(name, ty) | (_, Q.TermNode ty (Q.TypedGlobal _ name)) <- Q.termGraphNodes graph]
  let erasedEvidence = [() | (_, Q.TermNode _ form) <- Q.termGraphNodes graph,
        case form of
          Q.TypedForallIntroduction{} -> True
          Q.TypedImplicitTypeApplication{} -> True
          Q.TypedVisibleTypeApplication{} -> True
          Q.TypedContextIntroduction{} -> True
          Q.TypedContextApplication{} -> True
          _ -> False]
  assertEqual "monomorphic constructor invented type/dictionary evidence" [] erasedEvidence
