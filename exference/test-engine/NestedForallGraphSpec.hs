-- Independent source-checker controls for root and nested forall ownership.
-- The private test component owns the checker and rigid-scope entrances.
module NestedForallGraphSpec (tests, maybeEitherGraph) where

import Control.Monad (forM_)
import qualified Data.Set as Set
import qualified Language.Haskell.Exference.Core.Expression as E
import Language.Haskell.Exference.Core.FunctionBinding
  (EnvDictionary (..), FunctionBinding, functionBindingFromType)
import Language.Haskell.Exference.Core.Internal.ExpressionCheck
import Language.Haskell.Exference.Core.Internal.RigidScope
  (emptyRigidScope, nestedRigidProvenance)
import Language.Haskell.Exference.Core.RigidInstantiation
  (mkRigidInstantiationContext, planRigidInstantiation)
import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.Generated as G
import qualified Language.Haskell.Synthesis.Type as T
import qualified Language.Haskell.Synthesis.TypeAtom as A
import qualified Language.Haskell.Synthesis.TypedGenerated as Q
import qualified Language.Haskell.Synthesis.TypedGenerated.Haskell as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)

tests :: TestTree
tests = testGroup "Context-free forall graph authority"
  [ testCase "root opening retains the original source forall" $ do
      let goal = identity 0
          expression = E.ExpLambda 1 (TypeConstant 0) $ E.ExpVar 1 $ TypeConstant 0
      graph <- checkedGraph [] goal expression
      assertEqual "root forall witness was dropped" 1 $ length $ introductions graph
      assertRoot goal graph
      assertRenders graph
  , testCase "direct nested introduction retains both lexical binders" $ do
      let goal = TypeForall [0] [] $ TypeArrow (TypeVar 0) $ identity 2
          expression = E.ExpLambda 1 (TypeConstant 0) $
            E.ExpLambda 2 (TypeConstant 1) $ E.ExpVar 2 $ TypeConstant 1
      graph <- checkedGraph [] goal expression
      assertRoot goal graph
      assertEqual "root and nested openings were conflated" 2 $ length $ introductions graph
      assertRenders graph
  , testCase "application argument carries its own forall introduction" $ do
      let consumer = functionBindingFromType consumeName 0 $ TypeArrow (identity 2) token
          expression = E.ExpApply (E.ExpName consumeName) $
            E.ExpLambda 1 (TypeConstant 0) $ E.ExpVar 1 $ TypeConstant 0
      graph <- checkedGraph [consumer] token expression
      assertRoot token graph
      assertEqual "the argument forall was not retained" 1 $ length $ introductions graph
      assertRenders graph
  , testCase "annotation-only foreign rigid cannot impersonate either scope" $ do
      let directGoal = TypeForall [0] [] $ TypeArrow (TypeVar 0) $ identity 2
          direct = E.ExpLambda 1 (TypeConstant 0) $
            E.ExpLambda 2 (TypeConstant 7) $ E.ExpVar 2 $ TypeConstant 7
          consumer = functionBindingFromType consumeName 0 $ TypeArrow (identity 2) token
          argument = E.ExpApply (E.ExpName consumeName) $
            E.ExpLambda 1 (TypeConstant 7) $ E.ExpVar 1 $ TypeConstant 7
      rejectTypeMismatch [] directGoal direct
      rejectTypeMismatch [consumer] token argument
  , testCase "sibling introductions cannot share an annotation rigid" $ do
      let goal = TypeTuple Boxed [identity 2, identity 3]
          branch local rigid = E.ExpLambda local (TypeConstant rigid) $
            E.ExpVar local $ TypeConstant rigid
          good = E.ExpTuple [branch 1 0, branch 2 1]
          bad = E.ExpTuple [branch 1 0, branch 2 0]
      graph <- checkedGraph [] goal good
      assertRoot goal graph
      assertEqual "sibling introductions lost distinct ownership" 2 $
        Set.size $ Set.fromList $ introductions graph
      assertRenders graph
      rejectTypeMismatch [] goal bad
  , testCase "maybeEither exact source witness seals and renders without search" $ do
      graph <- maybeEitherGraph
      assertRoot maybeEitherType graph
      assertEqual "two root and three nested binders were not retained" 5 $
        length $ introductions graph
      assertEqual "a sibling borrowed a selected type variable" 5 $
        Set.size $ Set.fromList $ introductions graph
      assertEqual "a provider entered the closed witness" [] $
        G.expressionGlobals $ Q.eraseTermGraph graph
      assertRenders graph
  ]

identity :: TVarId -> HsType
identity binder = TypeForall [binder] [] $ TypeArrow (TypeVar binder) (TypeVar binder)

consumeName :: QualifiedName
consumeName = sourceName "consumePoly"

token :: HsType
token = TypeCons $ sourceName "Result"

sourceName :: String -> QualifiedName
sourceName spelling = either (error . show) id $ mkQualifiedName [] spelling

expectRight :: Show failure => Either failure value -> IO value
expectRight = either (fail . show) pure

checked :: [FunctionBinding] -> HsType -> E.Expression
  -> IO (Either ExpressionCheckError CheckedExpressionEvidence)
checked functions goal expression = do
  classes <- expectRight $ mkStaticClassEnv [] []
  plan <- expectRight $ planRigidInstantiation
    (mkRigidInstantiationContext $ EnvDictionary functions [] classes) [] goal
  context <- expectRight $ prepareExpressionCheckContext plan
    (mkQueryClassEnv classes []) functions [] goal
  pure $ checkExpressionInContextWithNestedRigidProvenanceEvidence
    context (nestedRigidProvenance emptyRigidScope) [] expression

checkedGraph :: [FunctionBinding] -> HsType -> E.Expression -> IO (Q.TermGraph HsType TVarId)
checkedGraph functions goal expression = do
  result <- checked functions goal expression >>= expectRight
  case checkedExpressionTermGraph 97 result of
    ExferenceTermGraphAvailable graph -> do
      assertEqual "graph changed the exact compatibility expression"
        (G.discardUnusedPatternBindingsBy id $ E.toGeneratedExpression expression)
        (Q.eraseTermGraph graph)
      pure graph
    unavailable -> fail $ "expected a complete plain source graph: " ++ show unavailable

rejectTypeMismatch :: [FunctionBinding] -> HsType -> E.Expression -> Assertion
rejectTypeMismatch functions goal expression = do
  result <- checked functions goal expression
  case result of
    Left TypeMismatch{} -> pure ()
    Left failure -> fail $ "expected the rigid type mismatch, got " ++ show failure
    Right _ -> fail "an unowned or sibling rigid acquired graph authority"

assertRoot :: HsType -> Q.TermGraph HsType TVarId -> Assertion
assertRoot expected graph = case Q.lookupTermNode (Q.termGraphRoot graph) graph of
  Nothing -> fail "sealed graph has no root"
  Just node -> do
    assertBool "root lost the exact original quantified source" $
      A.alphaEquivalentTypes expected $ Q.termNodeType node
    assertBool "closed source graph retained a free root skolem" $
      Set.null $ T.freeVariables $ Q.termNodeType node

introductions :: Q.TermGraph HsType TVarId -> [HsType]
introductions graph =
  [ Q.forallIntroductionVariable witness
  | (_, Q.TermNode _ (Q.TypedForallIntroduction _ _ witness)) <- Q.termGraphNodes graph ]

assertRenders :: Q.TermGraph HsType TVarId -> Assertion
assertRenders graph = do
  rendered <- expectRight $ H.renderHaskellTermGraph (G.defaultRenderOptions $ const "x") graph
  assertBool "typed renderer returned an empty source expression" $ not $ null rendered
  -- Rendering is a source artifact here, not a claim that GHC executed it.
  forM_ (introductions graph) $ \selected -> case selected of
    T.TypeVariable (T.RigidVariable _) -> pure ()
    _ -> fail "forall introduction did not select a proper rigid identity"

maybeEitherType :: HsType
maybeEitherType = TypeForall [0, 1] [] $
  TypeArrow (sumOfMaybes (TypeVar 0) (TypeVar 1)) $
    maybeType 6 $ eitherType 5 (TypeVar 0) (TypeVar 1)

maybeType :: TVarId -> HsType -> HsType
maybeType binder element = TypeForall [binder] [] $
  TypeArrow (TypeVar binder) $ TypeArrow (TypeArrow element $ TypeVar binder) $ TypeVar binder

eitherType :: TVarId -> HsType -> HsType -> HsType
eitherType binder left right = TypeForall [binder] [] $
  TypeArrow (TypeArrow left $ TypeVar binder) $
    TypeArrow (TypeArrow right $ TypeVar binder) $ TypeVar binder

sumOfMaybes :: HsType -> HsType -> HsType
sumOfMaybes left right = eitherType 4 (maybeType 2 left) (maybeType 3 right)

maybeEitherExpression :: E.Expression
maybeEitherExpression =
  E.ExpLambda 1 source $ E.ExpLambda 2 result $ E.ExpLambda 3 some $
    E.ExpApply (E.ExpApply (E.ExpVar 1 selected) leftBranch) rightBranch
 where
  left = TypeConstant 0
  right = TypeConstant 1
  result = TypeConstant 2
  source = sumOfMaybes left right
  some = TypeArrow (eitherType 5 left right) result
  selected = TypeArrow (TypeArrow (maybeType 2 left) result) $
    TypeArrow (TypeArrow (maybeType 3 right) result) result
  zero = E.ExpVar 2 result
  applySome payload = E.ExpApply (E.ExpVar 3 some) payload
  leftBranch = E.ExpLambda 4 (maybeType 2 left) $
    E.ExpApply (E.ExpApply (E.ExpVar 4 $ TypeArrow result $ TypeArrow (TypeArrow left result) result) zero) $
      E.ExpLambda 5 left $ applySome $
        E.ExpLambda 6 (TypeArrow left $ TypeConstant 3) $
          E.ExpLambda 7 (TypeArrow right $ TypeConstant 3) $
            E.ExpApply (E.ExpVar 6 $ TypeArrow left $ TypeConstant 3) $ E.ExpVar 5 left
  rightBranch = E.ExpLambda 8 (maybeType 3 right) $
    E.ExpApply (E.ExpApply (E.ExpVar 8 $ TypeArrow result $ TypeArrow (TypeArrow right result) result) zero) $
      E.ExpLambda 9 right $ applySome $
        E.ExpLambda 10 (TypeArrow left $ TypeConstant 4) $
          E.ExpLambda 11 (TypeArrow right $ TypeConstant 4) $
            E.ExpApply (E.ExpVar 11 $ TypeArrow right $ TypeConstant 4) $ E.ExpVar 9 right

maybeEitherGraph :: IO (Q.TermGraph HsType TVarId)
maybeEitherGraph = checkedGraph [] maybeEitherType maybeEitherExpression
