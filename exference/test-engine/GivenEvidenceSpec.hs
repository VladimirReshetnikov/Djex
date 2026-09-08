-- | Focused specification for the independently reconstructed Given path.
-- Private checker cases complement live search and native replay acceptance.
module GivenEvidenceSpec (tests) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import Language.Haskell.Exference.Core.Declaration
  (prepareSynthesisInventory, preparedSynthesisBackend, preparedSynthesisSchemes)
import Language.Haskell.Exference.Core.FunctionBinding
  (EnvDictionary (..), FunctionBinding (..))
import qualified Language.Haskell.Exference.Core.Expression as E
import Language.Haskell.Exference.Core.Internal.ExpressionCheck
import Language.Haskell.Exference.Core.Internal.RigidScope
  (emptyRigidScope, nestedRigidProvenance)
import Language.Haskell.Exference.Core.RigidInstantiation
  (mkRigidInstantiationContext, planRigidInstantiation)
import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.Declaration as D
import qualified Language.Haskell.Synthesis.Generated as G
import qualified Language.Haskell.Synthesis.Inventory as I
import qualified Language.Haskell.Synthesis.KindInference as K
import qualified Language.Haskell.Synthesis.TypeAtom as A
import qualified Language.Haskell.Synthesis.TypedGenerated as Q
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tests :: TestTree
tests = testGroup "Independent lexical Given evidence"
  [ testCase "unused root given retains the full qualified telescope" $ do
      classes <- unaryClasses
      let goal = TypeForall [0] [constraint 0] $ TypeArrow (TypeVar 0) (TypeVar 0)
          expression = E.ExpLambda 1 (TypeVar 0) $ E.ExpVar 1 $ TypeVar 0
      checked <- evidence classes [] Map.empty goal expression
      graph <- plainGraph expression checked
      assertRoot goal graph
      assertEqual "root dictionary slot was erased" [0] $ map Q.evidenceBinderSlot $ introductions graph
      assertEqual "unused given gained an application" [] $ applications graph
  , testCase "nested qualified callback retains its own unused given" $ do
      classes <- unaryClasses
      let callback = TypeForall [3] [constraint 3] $ TypeArrow (TypeVar 3) (TypeVar 3)
          consumer = TypeArrow callback tokenType
          goal = TypeArrow consumer tokenType
          expression = E.ExpLambda 1 consumer $ E.ExpApply (E.ExpVar 1 consumer) $
            E.ExpLambda 2 (TypeVar 7) $ E.ExpVar 2 $ TypeVar 7
      checked <- evidence classes [] Map.empty goal expression
      graph <- plainGraph expression checked
      assertEqual "nested context introduction was lost" [0] $ map Q.evidenceBinderSlot $ introductions graph
      assertEqual "callback ignored its given but acquired an application" [] $ applications graph
  , testCase "opaque forwarding keeps its complete contextual scheme" $ do
      classes <- unaryClasses
      let provider = providerScheme 3
          goal = TypeArrow provider $ providerScheme 9
          expression = E.ExpLambda 1 provider $ E.ExpVar 1 provider
      checked <- evidence classes [] Map.empty goal expression
      graph <- plainGraph expression checked
      assertEqual "opaque forwarding invented dictionary elimination" [] $ applications graph
      assertBool "the forwarded local lost its original scheme" $ any
        (\(_, Q.TermNode ty form) -> case form of
          Q.TypedLocal{} -> A.alphaEquivalentTypes provider ty
          _ -> False) $ Q.termGraphNodes graph
  , testCase "local application selects its exact root Given slot" $ do
      classes <- unaryClasses
      checked <- evidence classes [] Map.empty (localGoal [constraint 0]) localExpression
      graph <- plainGraph localExpression checked
      assertOneGivenApplication graph
      assertBool "local source scheme was flattened" $ any
        (\(_, Q.TermNode ty form) -> case form of
          Q.TypedLocal{} -> A.alphaEquivalentTypes (providerScheme 3) ty
          _ -> False) $ Q.termGraphNodes graph
  , testCase "global application retains its full source scheme" $ do
      classes <- unaryClasses
      (bindings, schemes) <- tokenEnvironment
      let goal = TypeForall [0] [constraint 0] $ TypeArrow (TypeVar 0) tokenType
          expression = E.ExpLambda 2 (TypeVar 0) $
            E.ExpApply (E.ExpName tokenName) $ E.ExpVar 2 $ TypeVar 0
      checked <- evidence classes bindings schemes goal expression
      graph <- plainGraph expression checked
      assertOneGivenApplication graph
      let globals = [(globalName, ty) | (_, Q.TermNode ty (Q.TypedGlobal _ globalName)) <- Q.termGraphNodes graph]
      assertEqual "global provider identity changed" [tokenName] $ map fst globals
      forM_ globals $ \(_, ty) -> assertBool "global provider lost its complete source telescope" $
        A.alphaEquivalentTypes (providerScheme 3) ty
  , testCase "a bare global method infers a constraint-only parameter independently" $ do
      classes <- unaryClasses
      let provider = TypeForall [3] [constraint 3] tokenType
          goal = TypeForall [0] [constraint 0] tokenType
          expression = E.ExpName tokenName
      (bindings, schemes) <- providerEnvironment provider
      checked <- evidence classes bindings schemes goal expression
      graph <- plainGraph expression checked
      assertRoot goal graph
      assertOneGivenApplication graph
      assertEqual "method lost its implicit selection" 1 $
        length [() | (_, Q.TermNode _ Q.TypedImplicitTypeApplication{}) <- Q.termGraphNodes graph]
  , testCase "a bare local method infers a constraint-only parameter independently" $ do
      classes <- unaryClasses
      let provider = TypeForall [3] [constraint 3] tokenType
          goal = TypeForall [0] [constraint 0] $ TypeArrow provider tokenType
          expression = E.ExpLambda 1 provider $ E.ExpVar 1 tokenType
      checked <- evidence classes [] Map.empty goal expression
      graph <- plainGraph expression checked
      assertRoot goal graph
      assertOneGivenApplication graph
  , testCase "constraint-only inference retains joint provider constraints" $ do
      let d ty = HsConstraint (name "D") [ty]
          provider = TypeForall [3] [constraint 3, d $ TypeVar 3] tokenType
          goal = TypeForall [0, 1] [constraint 0, constraint 1, d $ TypeVar 1] $
            TypeArrow provider tokenType
          expression = E.ExpLambda 1 provider $ E.ExpVar 1 tokenType
      classes <- expectRight $ mkStaticClassEnv
        [HsTypeClass className [0] [], HsTypeClass (name "D") [0] []] []
      checked <- evidence classes [] Map.empty goal expression
      graph <- plainGraph expression checked
      assertEqual "provider constraints chose inconsistent type parameters" [1, 2] $
        map Q.evidenceBinderSlot $ applications graph
  , testCase "an unconstrained method cannot borrow a sibling's dictionary" $ do
      classes <- unaryClasses
      let provider = TypeForall [3] [constraint 3] tokenType
          goal = TypeForall [0] [] $ TypeTuple Boxed
            [TypeForall [] [constraint 0] tokenType, tokenType]
          expression = E.ExpTuple [E.ExpName tokenName, E.ExpName tokenName]
      (bindings, schemes) <- providerEnvironment provider
      plan <- expectRight $ planRigidInstantiation
        (mkRigidInstantiationContext $ EnvDictionary bindings [] classes) [] goal
      context <- expectRight $ prepareExpressionCheckContextWithSchemes plan
        (mkQueryClassEnv classes []) bindings [] schemes goal
      case checkExpressionInContextWithNestedRigidProvenanceEvidence
          context (nestedRigidProvenance emptyRigidScope) [] expression of
        Left RefutableConstraints{} -> pure ()
        Left ConstraintMismatch{} -> pure ()
        Left failure -> fail $ "unexpected sibling rejection: " ++ show failure
        Right _ -> fail "sibling acquired method evidence"
  , testCase "equal root givens retain distinct ordered slots" $ do
      classes <- unaryClasses
      checked <- evidence classes [] Map.empty
        (localGoal [constraint 0, constraint 0]) localExpression
      graph <- plainGraph localExpression checked
      let binders = introductions graph
      assertEqual "equal source dictionaries were deduplicated" [0, 1] $ map Q.evidenceBinderSlot binders
      assertEqual "the direct Given choice lost its stable first source slot"
        (take 1 binders) $ applications graph
  , testCase "adjacent qualified layers retain separate introduction sites" $ do
      classes <- unaryClasses
      let goal = TypeForall [0] [constraint 0] $
            TypeForall [] [constraint 0] $ TypeArrow (TypeVar 0) (TypeVar 0)
          expression = E.ExpLambda 1 (TypeVar 0) $ E.ExpVar 1 $ TypeVar 0
      checked <- evidence classes [] Map.empty goal expression
      graph <- plainGraph expression checked
      let binders = introductions graph
      assertEqual "qualified layers were flattened or duplicated" [0, 0] $ map Q.evidenceBinderSlot binders
      case binders of
        [first, second] -> assertBool "two layers reused one introduction occurrence" $
          Q.evidenceBinderIntroduction first /= Q.evidenceBinderIntroduction second
        _ -> fail "expected two separate source context layers"
  , testCase "instance discharge does not become Given evidence" $ do
      classes <- expectRight $ mkStaticClassEnv [HsTypeClass className [0] []]
        [HsInstance [] $ HsConstraint className [integerType]]
      let goal = TypeArrow (providerScheme 3) $ TypeArrow integerType tokenType
          expression = E.ExpLambda 1 (providerScheme 3) $ E.ExpLambda 2 integerType $
            E.ExpApply (E.ExpVar 1 $ TypeArrow integerType tokenType) $ E.ExpVar 2 integerType
      checked <- evidence classes [] Map.empty goal expression
      unsupportedGiven checked
  , testCase "superclass closure does not become an exact Given" $ do
      let derived = name "D"
      classes <- expectRight $ mkStaticClassEnv
        [ HsTypeClass className [0] []
        , HsTypeClass derived [0] [constraint 0]
        ] []
      checked <- evidence classes [] Map.empty
        (localGoal [HsConstraint derived [TypeVar 0]]) localExpression
      unsupportedGiven checked
  , testCase "missing complete global scheme cannot erase its constraint" $ do
      classes <- unaryClasses
      let goal = TypeForall [] [HsConstraint className [integerType]] $
            TypeArrow integerType tokenType
          expression = E.ExpLambda 2 integerType $
            E.ExpApply (E.ExpName tokenName) $ E.ExpVar 2 integerType
      checked <- evidence classes [tokenBinding] Map.empty goal expression
      unsupportedGiven checked
  , testCase "contextual visible origins remain explicit unsupported graphs" $ do
      classes <- unaryClasses
      (bindings, schemes) <- tokenEnvironment
      selected <- expectRight $ G.specifiedVisibleTypeArgument integerType
      let goal = TypeForall [] [HsConstraint className [integerType]] $
            TypeArrow integerType tokenType
          expression = E.ExpLambda 2 integerType $
            E.ExpApply (E.ExpTypeApply (E.ExpName tokenName) selected) $ E.ExpVar 2 integerType
      checked <- evidence classes bindings schemes goal expression
      assertBool "unsupported graph silently dropped its original certificate origin" $
        not $ null $ checkedExpressionTypeApplicationOrigins checked
      case checkedExpressionTermGraph 11 checked of
        ExferenceTermGraphUnavailable UnsupportedContextualCertificateGraph -> pure ()
        other -> fail $ "expected the contextual certificate boundary, got " ++ show other
  , testCase "a sibling cannot consume a locally introduced dictionary" $ do
      classes <- unaryClasses
      let arrow = TypeArrow (TypeVar 0) tokenType
          goal = TypeForall [0] [] $ TypeTuple Boxed
            [TypeForall [] [constraint 0] arrow, arrow]
          body variable = E.ExpLambda variable (TypeVar 0) $
            E.ExpApply (E.ExpName tokenName) $ E.ExpVar variable $ TypeVar 0
      case checkExpression (mkQueryClassEnv classes []) [tokenBinding] [] goal [] $
          E.ExpTuple [body 1, body 2] of
        Left ConstraintMismatch{} -> pure ()
        Left RefutableConstraints{} -> pure ()
        other -> fail $ "expected a scoped constraint failure, got " ++ show other
  ]

className, tokenName :: QualifiedName
className = name "C"
tokenName = name "token"

tokenType, integerType :: HsType
tokenType = TypeCons $ name "Token"
integerType = TypeCons $ name "Int"

constraint :: TVarId -> HsConstraint
constraint variable = HsConstraint className [TypeVar variable]

providerScheme :: TVarId -> HsType
providerScheme variable = TypeForall [variable] [constraint variable] $
  TypeArrow (TypeVar variable) tokenType

tokenBinding :: FunctionBinding
tokenBinding = FunctionBinding tokenType tokenName 0 [constraint 3] [TypeVar 3]

-- The declaration adapter owns the flat binding's opened variable namespace.
-- Derive both views from one source declaration instead of pairing a manually
-- numbered compatibility binding with an independently retained source scheme.
tokenEnvironment :: IO ([FunctionBinding], Map.Map QualifiedName HsType)
tokenEnvironment = providerEnvironment $ providerScheme 3

providerEnvironment :: HsType -> IO ([FunctionBinding], Map.Map QualifiedName HsType)
providerEnvironment provider = do
  inventory <- expectRight $ I.mkInventory K.OpenKindInventory
    ([ D.ValueDeclaration $ D.ValueSignature () tokenName provider
     ] :: [D.Declaration SynthesisVariable Void ()])
  prepared <- expectRight $ prepareSynthesisInventory inventory
  pure
    ( environmentFunctions $ preparedSynthesisBackend prepared
    , preparedSynthesisSchemes prepared
    )

localGoal :: [HsConstraint] -> HsType
localGoal constraints = TypeForall [0] constraints $
  TypeArrow (providerScheme 3) $ TypeArrow (TypeVar 0) tokenType

localExpression :: E.Expression
localExpression = E.ExpLambda 1 (providerScheme 3) $ E.ExpLambda 2 (TypeVar 0) $
  E.ExpApply (E.ExpVar 1 $ TypeArrow (TypeVar 0) tokenType) $ E.ExpVar 2 $ TypeVar 0

unaryClasses :: IO StaticClassEnv
unaryClasses = expectRight $ mkStaticClassEnv [HsTypeClass className [0] []] []

evidence :: StaticClassEnv -> [FunctionBinding] -> Map.Map QualifiedName HsType
  -> HsType -> E.Expression -> IO CheckedExpressionEvidence
evidence classes functions schemes goal expression = do
  plan <- expectRight $ planRigidInstantiation
    (mkRigidInstantiationContext $ EnvDictionary functions [] classes) [] goal
  context <- expectRight $ prepareExpressionCheckContextWithSchemes plan
    (mkQueryClassEnv classes []) functions [] schemes goal
  expectRight $ checkExpressionInContextWithNestedRigidProvenanceEvidence
    context (nestedRigidProvenance emptyRigidScope) [] expression

plainGraph :: E.Expression -> CheckedExpressionEvidence
  -> IO (Q.TermGraph HsType TVarId)
plainGraph expression checked = case checkedExpressionTermGraph 11 checked of
  ExferenceTermGraphAvailable graph -> do
    assertEqual "graph erasure changed the checked expression"
      (G.discardUnusedPatternBindingsBy id $ E.toGeneratedExpression expression) $
      Q.eraseTermGraph graph
    pure graph
  other -> fail $ "expected complete plain graph evidence, got " ++ show other

assertRoot :: HsType -> Q.TermGraph HsType TVarId -> IO ()
assertRoot expected graph = case Q.lookupTermNode (Q.termGraphRoot graph) graph of
  Nothing -> fail "missing graph root"
  Just node -> assertBool "graph lost the original qualified goal" $
    A.alphaEquivalentTypes expected $ Q.termNodeType node

introductions :: Q.TermGraph HsType TVarId -> [Q.EvidenceBinderId]
introductions graph =
  [ Q.contextEvidenceBinder $ Q.givenContextEvidence occurrence slot
  | (_, Q.TermNode _ (Q.TypedContextIntroduction occurrence _ witness)) <- Q.termGraphNodes graph
  , slot <- take (length $ Q.contextIntroductionConstraints witness) [0 ..]
  ]

applications :: Q.TermGraph HsType TVarId -> [Q.EvidenceBinderId]
applications graph =
  [ Q.contextEvidenceBinder proof
  | (_, Q.TermNode _ (Q.TypedContextApplication _ _ witness)) <- Q.termGraphNodes graph
  , proof <- Q.contextApplicationEvidence witness
  ]

assertOneGivenApplication :: Q.TermGraph HsType TVarId -> IO ()
assertOneGivenApplication graph = do
  let binders = introductions graph
  assertEqual "expected one source dictionary slot" [0] $ map Q.evidenceBinderSlot binders
  assertEqual "application did not retain the actual introduction/slot identity"
    binders $ applications graph

unsupportedGiven :: CheckedExpressionEvidence -> IO ()
unsupportedGiven checked = case checkedExpressionTermGraph 11 checked of
  ExferenceTermGraphUnavailable UnsupportedContextEvidence{} -> pure ()
  other -> fail $ "expected unsupported non-Given evidence, got " ++ show other

name :: String -> QualifiedName
name spelling = either (error . show) id $ mkQualifiedName [] spelling

expectRight :: Show error => Either error value -> IO value
expectRight = either (fail . show) pure
