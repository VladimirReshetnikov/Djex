{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.DeepSeq (force)
import Control.Exception
  ( SomeException
  , displayException
  , evaluate
  , try
  )
import Data.List (isInfixOf)

import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Internal.TypedGenerated.Certificate
import Language.Haskell.Synthesis.Name
  ( Boxity (Boxed)
  , Name
  , functionName
  , parseName
  , tupleName
  )
import Language.Haskell.Synthesis.Type
  ( Type (..)
  , TypeError (..)
  , Variable (..)
  )
import Language.Haskell.Synthesis.TypedGenerated
  ( CertificateId
  , certificateId
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
  ( assertFailure
  , testCase
  , (@?=)
  )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "bounded type-application certificate plans"
  [ limitTests
  , tableShapeTests
  , replayTests
  , contextualObligationTests
  , namespaceTests
  , demandTests
  ]

limitTests :: TestTree
limitTests = testGroup "limits"
  [ testCase "validate independent signed limits in field order" $ do
      mkTypeApplicationCertificateLimits (-1) (-2) (-3) (-4) (-5) @?=
        Left (NegativeTypeApplicationCertificateLimit
          TypeApplicationCertificateEntries (-1))
      mkTypeApplicationCertificateLimits 1 (-2) (-3) (-4) (-5) @?=
        Left (NegativeTypeApplicationCertificateLimit
          TypeApplicationCertificateSelections (-2))
      mkTypeApplicationCertificateLimits 1 2 (-3) (-4) (-5) @?=
        Left (NegativeTypeApplicationCertificateLimit
          TypeApplicationCertificateObligations (-3))
      mkTypeApplicationCertificateLimits 1 2 3 (-4) (-5) @?=
        Left (NegativeTypeApplicationCertificateLimit
          TypeApplicationCertificateTypeNodes (-4))
      mkTypeApplicationCertificateLimits 1 2 3 4 (-5) @?=
        Left (NegativeTypeApplicationCertificateLimit
          TypeApplicationCertificateCollectionWidth (-5))
  , testCase "expose exact validated limits" $ do
      limits <- expectRight $ mkTypeApplicationCertificateLimits 1 2 3 4 5
      ( maximumTypeApplicationCertificates limits
        , maximumTypeApplicationCertificateSelections limits
        , maximumTypeApplicationCertificateObligations limits
        , maximumTypeApplicationCertificateTypeNodes limits
        , maximumTypeApplicationCertificateCollectionWidth limits
        ) @?= (1, 2, 3, 4, 5)
  ]

tableShapeTests :: TestTree
tableShapeTests = testGroup "table shape"
  [ testCase "accept an empty table" $ do
      table <- expectRight $ seal []
      checkedTypeApplicationCertificateCount table @?= 0
      case lookupCheckedTypeApplicationCertificatePlan cert7 table of
        Nothing -> pure ()
        Just _ -> assertFailure "empty certificate table returned a row"
  , testCase "ignore row order while retaining coordinate lookup" $ do
      let first = oneBinderSource cert7 intType
          second = oneBinderSource cert900 boolType
      left <- expectRight $ seal [first, second]
      right <- expectRight $ seal [second, first]
      leftFirst <- expectPlanFrom cert7 left
      rightFirst <- expectPlanFrom cert7 right
      leftSecond <- expectPlanFrom cert900 left
      rightSecond <- expectPlanFrom cert900 right
      assertPlanStepsEqual leftFirst rightFirst
      assertPlanStepsEqual leftSecond rightSecond
  , testCase "bound the outer spine before row payloads" $ do
      limits <- expectRight $ mkTypeApplicationCertificateLimits 0 4 4 64 16
      assertLeft (TypeApplicationCertificateEntryLimitExceeded 0 1)
        $ sealWith limits [error "outer-entry-payload"]
  , testCase "reject duplicate coordinates before duplicate payloads" $ do
      let duplicate = TypeApplicationCertificateSource cert7
            (error "duplicate-scheme") (error "duplicate-selections")
      assertLeft (DuplicateTypeApplicationCertificateId cert7)
        $ sealTypeApplicationCertificateTable
          defaultTypeApplicationCertificateLimits
          [oneBinderSource cert7 intType, duplicate]
  , testCase "reject monotypes and short or long selection vectors" $ do
      assertLeft (TypeApplicationCertificateHasNoLeadingBinder cert7)
        $ seal [TypeApplicationCertificateSource cert7 intType []]
      assertLeft (TypeApplicationCertificateSelectionArityMismatch cert7 2 1)
        $ seal [twoBinderSource cert7 [intType]]
      assertLeft (TypeApplicationCertificateSelectionArityMismatch cert7 2 3)
        $ seal [twoBinderSource cert7 [intType, boolType, charType]]
  , testCase "bound selection and telescope spines productively" $ do
      limits <- expectRight $ mkTypeApplicationCertificateLimits 2 1 4 64 16
      assertLeft (TypeApplicationCertificateSelectionLimitExceeded cert7 1 2)
        $ sealWith limits
          [twoBinderSource cert7 [error "first-selection", error "second"]]
      assertLeft (TypeApplicationCertificateTelescopeLimitExceeded cert7 1 2)
        $ sealWith limits [twoBinderSource cert7 [intType]]
  , testCase "terminate on cyclic outer, selection, and type structures" $ do
      outerLimits <- expectRight
        $ mkTypeApplicationCertificateLimits 1 4 4 64 16
      let outer = oneBinderSource cert7 intType : outer
      assertLeft (TypeApplicationCertificateEntryLimitExceeded 1 2)
        $ sealWith outerLimits outer

      selectionLimits <- expectRight
        $ mkTypeApplicationCertificateLimits 1 1 4 64 16
      let cyclicSelections = intType : cyclicSelections
      assertLeft (TypeApplicationCertificateSelectionLimitExceeded cert7 1 2)
        $ sealWith selectionLimits
          [TypeApplicationCertificateSource cert7
            (ForallType [FlexibleVariable "a"] [] intType)
            cyclicSelections]

      typeLimits <- expectRight
        $ mkTypeApplicationCertificateLimits 1 1 4 2 16
      let cyclicType = TypeApplication cyclicType intType
      assertLeft
        (TypeApplicationCertificateTypeNodeLimitExceeded
          (TypeApplicationCertificateSelectionType cert7 0) 2 3)
        $ sealWith typeLimits [oneBinderSource cert7 cyclicType]
  , testCase "report exact source type-bound and syntax sites" $ do
      nodeLimits <- expectRight
        $ mkTypeApplicationCertificateLimits 1 1 4 1 16
      assertLeft
        (TypeApplicationCertificateTypeNodeLimitExceeded
          (TypeApplicationCertificateSchemeType cert7) 1 2)
        $ sealWith nodeLimits [oneBinderSource cert7 intType]

      widthLimits <- expectRight
        $ mkTypeApplicationCertificateLimits 1 1 4 64 0
      assertLeft
        (TypeApplicationCertificateTypeCollectionLimitExceeded
          (TypeApplicationCertificateSchemeType cert7) 0 1)
        $ sealWith widthLimits [oneBinderSource cert7 intType]

      let invalidName = parsedName "Fixture.value"
      assertLeft
        (InvalidTypeApplicationCertificateType
          (TypeApplicationCertificateSchemeType cert7)
          (InvalidTypeConstructor invalidName))
        $ seal [TypeApplicationCertificateSource cert7
          (ForallType [FlexibleVariable "a"] []
            $ TypeConstructor invalidName)
          [intType]]
  ]

replayTests :: TestTree
replayTests = testGroup "structural replay"
  [ testCase "derive a complete two-slot substitution chain" $ do
      plan <- sealPlan $ twoBinderSource cert7 [intType, boolType]
      checkedTypeApplicationCertificateStepCount plan @?= 2
      checkedTypeApplicationCertificateObligationCount plan @?= 0
      (first, second) <- expectTwo
        $ checkedTypeApplicationCertificateSteps plan
      map checkedTypeApplicationCertificateStepSlot [first, second] @?= [0, 1]
      checkedTypeApplicationCertificateStepSelected first @?=
        freePlanType intType
      checkedTypeApplicationCertificateStepResult second @?=
        pairPlanType intType boolType
  , testCase "activate same-layer obligations only at its final slot" $ do
      classC <- expectName "Fixture.C"
      classD <- expectName "Fixture.D"
      pair <- expectName "Fixture.Pair"
      let a = FlexibleVariable "a"
          b = FlexibleVariable "b"
          scheme = ForallType [a, b]
            [ Constraint classC [TypeVariable a]
            , Constraint classD [TypeVariable b]
            ]
            $ apply2 pair (TypeVariable a) (TypeVariable b)
      plan <- sealPlan $ TypeApplicationCertificateSource cert7 scheme
        [intType, boolType]
      (first, second) <- expectTwo
        $ checkedTypeApplicationCertificateSteps plan
      checkedTypeApplicationCertificateStepObligationCount first @?= 0
      checkedTypeApplicationCertificateStepObligationCount second @?= 2
      map constraintClassName
        (checkedTypeApplicationCertificateStepObligations second) @?=
          [classC, classD]
  , testCase "retain free selected variables without inventing closure proof" $ do
      let free = RigidVariable "query-rigid"
      plan <- sealPlan $ oneBinderSource cert7 $ TypeVariable free
      step <- expectOne $ checkedTypeApplicationCertificateSteps plan
      checkedTypeApplicationCertificateStepSelected step @?=
        TypeVariable (TypeApplicationCertificateFree free)
  , testCase "retain free scheme variables without inventing provenance proof" $ do
      let free = RigidVariable "inventory-rigid"
          binder = FlexibleVariable "a"
          scheme = ForallType [binder] []
            $ FunctionType (TypeVariable free) (TypeVariable binder)
      plan <- sealPlan $ TypeApplicationCertificateSource cert7 scheme
        [intType]
      step <- expectOne $ checkedTypeApplicationCertificateSteps plan
      checkedTypeApplicationCertificateStepResult step @?=
        FunctionType
          (TypeVariable $ TypeApplicationCertificateFree free)
          (freePlanType intType)
  , testCase "consume vacuous binders as explicit semantic slots" $ do
      let binder = FlexibleVariable "unused"
          scheme = ForallType [binder] [] intType
      plan <- sealPlan $ TypeApplicationCertificateSource cert7 scheme
        [boolType]
      step <- expectOne $ checkedTypeApplicationCertificateSteps plan
      checkedTypeApplicationCertificateStepSlot step @?= 0
      checkedTypeApplicationCertificateStepSelected step @?=
        freePlanType boolType
      checkedTypeApplicationCertificateStepResult step @?=
        freePlanType intType
  , testCase "canonicalize a newly saturated function result" $ do
      let binder = FlexibleVariable "f"
          scheme = ForallType [binder] []
            $ TypeApplication (TypeVariable binder) intType
          partialFunction = TypeApplication
            (TypeConstructor functionName) boolType
      plan <- sealPlan $ TypeApplicationCertificateSource cert7 scheme
        [partialFunction]
      step <- expectOne $ checkedTypeApplicationCertificateSteps plan
      checkedTypeApplicationCertificateStepResult step @?=
        FunctionType (freePlanType boolType) (freePlanType intType)
  , testCase "canonicalize a newly saturated tuple obligation" $ do
      classC <- expectName "Fixture.C"
      pairConstructor <- expectRight $ tupleName Boxed 2
      let binder = FlexibleVariable "f"
          applied = TypeApplication (TypeVariable binder) intType
          scheme = ForallType [binder] [Constraint classC [applied]] intType
          partialTuple = TypeApplication
            (TypeConstructor pairConstructor) boolType
      plan <- sealPlan $ TypeApplicationCertificateSource cert7 scheme
        [partialTuple]
      step <- expectOne $ checkedTypeApplicationCertificateSteps plan
      checkedTypeApplicationCertificateStepObligations step @?=
        [Constraint classC
          [TupleType Boxed [freePlanType boolType, freePlanType intType]]]
  ]

contextualObligationTests :: TestTree
contextualObligationTests = testGroup "context activation"
  [ testCase "derive contexts present in the source scheme syntax" $ do
      classC <- expectName "Fixture.C"
      let a = FlexibleVariable "a"
          scheme = ForallType [a]
            [Constraint classC [TypeVariable a]] $ TypeVariable a
      plan <- sealPlan $ TypeApplicationCertificateSource cert7 scheme [intType]
      step <- expectOne $ checkedTypeApplicationCertificateSteps plan
      checkedTypeApplicationCertificateStepResult step @?= freePlanType intType
      checkedTypeApplicationCertificateStepObligationCount step @?= 1
  , testCase "never turn selection-contained contexts into source obligations" $ do
      classC <- expectName "Fixture.C"
      let a = FlexibleVariable "a"
          selected = ForallType [] [Constraint classC [intType]] intType
          scheme = ForallType [a] [] $ TypeVariable a
      plan <- sealPlan $ TypeApplicationCertificateSource cert7 scheme [selected]
      step <- expectOne $ checkedTypeApplicationCertificateSteps plan
      checkedTypeApplicationCertificateStepResult step @?=
        checkedTypeApplicationCertificateStepSelected step
      checkedTypeApplicationCertificateStepObligationCount step @?= 0
  , testCase "substitute a contextual selection into a scheme context" $ do
      classC <- expectName "Fixture.C"
      classD <- expectName "Fixture.D"
      let sourceBinder = FlexibleVariable "source"
          selectedBinder = FlexibleVariable "selected"
          selected = ForallType [selectedBinder]
            [Constraint classD [TypeVariable selectedBinder]]
            $ TypeVariable selectedBinder
          scheme = ForallType [sourceBinder]
            [Constraint classC [TypeVariable sourceBinder]]
            $ TypeVariable sourceBinder
      plan <- sealPlan $ TypeApplicationCertificateSource cert7 scheme
        [selected]
      step <- expectOne $ checkedTypeApplicationCertificateSteps plan
      let preparedSelected =
            checkedTypeApplicationCertificateStepSelected step
      checkedTypeApplicationCertificateStepResult step @?= preparedSelected
      checkedTypeApplicationCertificateStepObligations step @?=
        [Constraint classC [preparedSelected]]
  , testCase "preserve outer and nested source-context order" $ do
      classC <- expectName "Fixture.C"
      classD <- expectName "Fixture.D"
      let a = FlexibleVariable "a"
          b = FlexibleVariable "b"
          scheme = ForallType [] [Constraint classC [intType]]
            $ ForallType [a] []
            $ ForallType [] [Constraint classD [TypeVariable a]]
            $ ForallType [b] [] $ TypeVariable b
      plan <- sealPlan $ TypeApplicationCertificateSource cert7 scheme
        [intType, boolType]
      (first, second) <- expectTwo
        $ checkedTypeApplicationCertificateSteps plan
      checkedTypeApplicationCertificateStepObligationCount first @?= 0
      map constraintClassName
        (checkedTypeApplicationCertificateStepObligations second) @?=
          [classC, classD]
  ]

namespaceTests :: TestTree
namespaceTests = testGroup "canonical namespaces"
  [ testCase "ignore bound spelling while retaining table coordinates only for lookup" $ do
      let source binder certificate = TypeApplicationCertificateSource
            certificate (ForallType [binder] [] $ TypeVariable binder) [intType]
      left <- sealPlan $ source (FlexibleVariable "alpha") cert7
      right <- sealPlan $ source (FlexibleVariable "renamed") cert900
      assertPlanStepsEqual left right
  , testCase "erase vacuous foralls before assigning private scopes" $ do
      let binder = FlexibleVariable "a"
          source scheme = TypeApplicationCertificateSource cert7 scheme
            [intType]
          plain = ForallType [binder] [] $ TypeVariable binder
          wrapped = ForallType [] [] plain
      left <- sealPlan $ source plain
      right <- sealPlan $ source wrapped
      assertPlanStepsEqual left right
  , testCase "ignore selected binder spelling" $ do
      let binder = FlexibleVariable "a"
          selected selectedBinder = ForallType [selectedBinder] []
            $ TypeVariable selectedBinder
          source selection = TypeApplicationCertificateSource cert7
            (ForallType [binder] [] $ TypeVariable binder) [selection]
      left <- sealPlan $ source $ selected $ FlexibleVariable "inner"
      right <- sealPlan $ source $ selected $ FlexibleVariable "renamed"
      assertPlanStepsEqual left right
  , testCase "retain nominal free-variable identity" $ do
      left <- sealPlan $ oneBinderSource cert7
        $ TypeVariable $ RigidVariable "left"
      right <- sealPlan $ oneBinderSource cert900
        $ TypeVariable $ RigidVariable "right"
      leftStep <- expectOne $ checkedTypeApplicationCertificateSteps left
      rightStep <- expectOne $ checkedTypeApplicationCertificateSteps right
      if checkedTypeApplicationCertificateStepSelected leftStep ==
          checkedTypeApplicationCertificateStepSelected rightStep
        then assertFailure "distinct nominal free variables collapsed"
        else pure ()
  , testCase "keep source and selected binders in disjoint namespaces" $ do
      let same = FlexibleVariable "same"
          selected = ForallType [same] [] $ TypeVariable same
          source = TypeApplicationCertificateSource cert7
            (ForallType [same] [] $ TypeVariable same) [selected]
      plan <- sealPlan source
      step <- expectOne $ checkedTypeApplicationCertificateSteps plan
      case checkedTypeApplicationCertificateStepResult step of
        ForallType [TypeApplicationCertificateSelectionBound 0 0 0] []
            (TypeVariable
              (TypeApplicationCertificateSelectionBound 0 0 0)) -> pure ()
        other -> assertFailure $ "selected binder was captured: " ++ show other
  , testCase "do not capture a free selection under a same-spelled source binder" $ do
      let firstBinder = FlexibleVariable "a"
          remainingBinder = FlexibleVariable "b"
          scheme = ForallType [firstBinder, remainingBinder] []
            $ TypeVariable firstBinder
          selectedFree = TypeVariable remainingBinder
      plan <- sealPlan $ TypeApplicationCertificateSource cert7 scheme
        [selectedFree, intType]
      (first, _) <- expectTwo
        $ checkedTypeApplicationCertificateSteps plan
      checkedTypeApplicationCertificateStepResult first @?=
        ForallType [TypeApplicationCertificateSourceBound 0 1] []
          (TypeVariable
            $ TypeApplicationCertificateFree remainingBinder)
  ]

demandTests :: TestTree
demandTests = testGroup "productive demand"
  [ testCase "selection arity does not demand elements" $ do
      let source = twoBinderSource cert7 [error "selection-element"]
      assertLeft (TypeApplicationCertificateSelectionArityMismatch cert7 2 1)
        $ seal [source]
  , testCase "obligation count wins before excess obligation payload" $ do
      classC <- expectName "Fixture.C"
      classD <- expectName "Fixture.D"
      pair <- expectName "Fixture.Pair"
      let a = FlexibleVariable "a"
          largeSelection = FunctionType
            (FunctionType intType intType)
            (FunctionType intType intType)
          scheme = ForallType [a]
            [ Constraint classC
                [apply2 pair (TypeVariable a) (TypeVariable a)]
            , Constraint classD [intType]
            ] intType
      limits <- expectRight $ mkTypeApplicationCertificateLimits 1 1 1 8 16
      assertLeft (TypeApplicationCertificateObligationLimitExceeded cert7 1 2)
        $ sealWith limits
          [TypeApplicationCertificateSource cert7 scheme [largeSelection]]
      payloadLimits <- expectRight
        $ mkTypeApplicationCertificateLimits 1 1 2 8 16
      assertLeft
        (TypeApplicationCertificateTypeNodeLimitExceeded
          (TypeApplicationCertificateDerivedObligationType cert7 0 0 0)
          8 9)
        $ sealWith payloadLimits
          [TypeApplicationCertificateSource cert7 scheme [largeSelection]]
  , testCase "accepted plans have an honest deep NFData boundary" $ do
      table <- expectRight $ seal [oneBinderSource cert7 intType]
      _ <- evaluate $ force table
      pure ()
  , testCase "construction stays productive while deep forcing reaches identities" $ do
      let poison = RigidVariable $ error "retained-certificate-variable"
      admitted <- evaluate $ seal
        [oneBinderSource cert7 $ TypeVariable poison]
      table <- expectRight admitted
      attempted <- try $ evaluate $ force table
      case attempted of
        Left failure
          | "retained-certificate-variable" `isInfixOf`
              displayException (failure :: SomeException) -> pure ()
          | otherwise -> assertFailure $ "unexpected deep-force failure: " ++
              displayException failure
        Right _ -> assertFailure "deep force left a retained identity lazy"
  ]

seal
  :: [TypeApplicationCertificateSource TestVariable]
  -> Either (TypeApplicationCertificateError TestVariable)
      (CheckedTypeApplicationCertificateTable TestVariable)
seal = sealWith defaultTypeApplicationCertificateLimits

sealWith
  :: TypeApplicationCertificateLimits
  -> [TypeApplicationCertificateSource TestVariable]
  -> Either (TypeApplicationCertificateError TestVariable)
      (CheckedTypeApplicationCertificateTable TestVariable)
sealWith = sealTypeApplicationCertificateTable

sealPlan
  :: TypeApplicationCertificateSource TestVariable
  -> IO (CheckedTypeApplicationCertificatePlan TestVariable)
sealPlan source = do
  table <- expectRight $ seal [source]
  expectPlanFrom (typeApplicationCertificateSourceId source) table

expectPlanFrom
  :: CertificateId
  -> CheckedTypeApplicationCertificateTable variable
  -> IO (CheckedTypeApplicationCertificatePlan variable)
expectPlanFrom certificate table =
  case lookupCheckedTypeApplicationCertificatePlan certificate table of
    Nothing -> assertFailure "checked certificate table lost its row"
    Just plan -> pure plan

oneBinderSource
  :: CertificateId
  -> TestType
  -> TypeApplicationCertificateSource TestVariable
oneBinderSource certificate selected =
  TypeApplicationCertificateSource certificate
    (ForallType [FlexibleVariable "a"] []
      $ TypeVariable $ FlexibleVariable "a")
    [selected]

twoBinderSource
  :: CertificateId
  -> [TestType]
  -> TypeApplicationCertificateSource TestVariable
twoBinderSource certificate selections =
  TypeApplicationCertificateSource certificate
    (ForallType [FlexibleVariable "a", FlexibleVariable "b"] []
      $ apply2 pairName
          (TypeVariable $ FlexibleVariable "a")
          (TypeVariable $ FlexibleVariable "b"))
    selections

type TestVariable = Variable String
type TestType = Type TestVariable

cert7, cert900 :: CertificateId
cert7 = certificateId 7
cert900 = certificateId 900

intType, boolType, charType :: TestType
intType = TypeConstructor intName
boolType = TypeConstructor boolName
charType = TypeConstructor charName

intName, boolName, charName, pairName :: Name
intName = parsedName "Int"
boolName = parsedName "Bool"
charName = parsedName "Char"
pairName = parsedName "Fixture.Pair"

apply2 :: Name -> TestType -> TestType -> TestType
apply2 name left right = TypeApplication
  (TypeApplication (TypeConstructor name) left) right

freePlanType
  :: TestType
  -> Type (TypeApplicationCertificatePlanVariable TestVariable)
freePlanType = fmap TypeApplicationCertificateFree

pairPlanType
  :: TestType
  -> TestType
  -> Type (TypeApplicationCertificatePlanVariable TestVariable)
pairPlanType left right = apply2Plan pairName
  (freePlanType left) (freePlanType right)

apply2Plan
  :: Name
  -> Type variable
  -> Type variable
  -> Type variable
apply2Plan name left right = TypeApplication
  (TypeApplication (TypeConstructor name) left) right

constraintClassName
  :: Constraint (Type variable)
  -> Name
constraintClassName (Constraint className _) = className

parsedName :: String -> Name
parsedName source = case parseName source of
  Left failure -> error $ "invalid test name: " ++ show failure
  Right name -> name

expectName :: String -> IO Name
expectName = pure . parsedName

expectRight :: Show failure => Either failure value -> IO value
expectRight source = case source of
  Left failure -> assertFailure $ "expected Right, got: " ++ show failure
  Right value -> pure value

assertLeft
  :: (Eq failure, Show failure)
  => failure
  -> Either failure value
  -> IO ()
assertLeft expected source = case source of
  Left actual -> actual @?= expected
  Right _ -> assertFailure "expected Left, got Right"

expectOne :: [value] -> IO value
expectOne values = case values of
  [value] -> pure value
  _ -> assertFailure "expected exactly one checked certificate step"

expectTwo :: [value] -> IO (value, value)
expectTwo values = case values of
  [first, second] -> pure (first, second)
  _ -> assertFailure "expected exactly two checked certificate steps"

assertPlanStepsEqual
  :: (Eq variable, Show variable)
  => CheckedTypeApplicationCertificatePlan variable
  -> CheckedTypeApplicationCertificatePlan variable
  -> IO ()
assertPlanStepsEqual left right = do
  checkedTypeApplicationCertificateStepCount left @?=
    checkedTypeApplicationCertificateStepCount right
  compareSteps
    (checkedTypeApplicationCertificateSteps left)
    (checkedTypeApplicationCertificateSteps right)
 where
  compareSteps [] [] = pure ()
  compareSteps (leftStep : leftRemaining) (rightStep : rightRemaining) = do
    checkedTypeApplicationCertificateStepSlot leftStep @?=
      checkedTypeApplicationCertificateStepSlot rightStep
    checkedTypeApplicationCertificateStepSource leftStep @?=
      checkedTypeApplicationCertificateStepSource rightStep
    checkedTypeApplicationCertificateStepSelected leftStep @?=
      checkedTypeApplicationCertificateStepSelected rightStep
    checkedTypeApplicationCertificateStepResult leftStep @?=
      checkedTypeApplicationCertificateStepResult rightStep
    checkedTypeApplicationCertificateStepObligations leftStep @?=
      checkedTypeApplicationCertificateStepObligations rightStep
    compareSteps leftRemaining rightRemaining
  compareSteps _ _ = assertFailure "checked certificate step counts disagreed"
