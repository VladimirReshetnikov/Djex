module ClassResolutionSpec (classResolutionTests) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Void (Void)

import Language.Haskell.Synthesis.Constraint (Constraint (..))
import qualified Language.Haskell.Synthesis.Declaration as Declaration
import Language.Haskell.Synthesis.Internal.ClassResolution
import qualified Language.Haskell.Synthesis.Inventory as Inventory
import qualified Language.Haskell.Synthesis.Kind as Kind
import qualified Language.Haskell.Synthesis.KindInference as KindInference
import Language.Haskell.Synthesis.Name
import qualified Language.Haskell.Synthesis.Type as Type
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

classResolutionTests :: TestTree
classResolutionTests = testGroup "checked class resolution"
  [ testCase "retain exact proof order behind replay" $ do
      environment <- sealDeclarations defaultClassResolutionLimits
        $ successDeclarations ()
      receipt <- expectDischarge
        $ dischargeGroundConstraint environment derivedInteger
      checkedConstraintDischargeGoal receipt @?= derivedInteger
      proof <- expectReplay
        $ replayCheckedConstraintDischarge environment derivedInteger receipt
      classResolutionProofGoal proof @?= derivedInteger
      classResolutionProofInstanceHead proof @?= derivedVariable
      let prerequisites = classResolutionProofPrerequisites proof
      map classResolutionProofGoal prerequisites @?=
        [leftInteger, rightInteger, baseInteger]
      map classResolutionProofInstanceHead prerequisites @?=
        [leftInteger, rightInteger, baseInteger]
  , testCase "resolve shrinking instances and stop current-path cycles" $ do
      environment <- sealDeclarations defaultClassResolutionLimits
        $ recursiveDeclarations ()
      receipt <- expectDischarge
        $ dischargeGroundConstraint environment recursiveNestedInteger
      proof <- expectReplay $ replayCheckedConstraintDischarge
        environment recursiveNestedInteger receipt
      proofGoalsPreorder proof @?=
        [recursiveNestedInteger, recursiveListInteger, recursiveInteger]
      cyclicEnvironment <- sealDeclarations defaultClassResolutionLimits
        $ cyclicDeclarations ()
      expectNoDischarge
        $ dischargeGroundConstraint cyclicEnvironment recursiveInteger
  , testCase "match repeated binders and higher-kinded applications jointly" $ do
      environment <- sealDeclarations defaultClassResolutionLimits
        $ matcherDeclarations ()
      repeated <- expectDischarge
        $ dischargeGroundConstraint environment sameIntegerInteger
      repeatedProof <- expectReplay $ replayCheckedConstraintDischarge
        environment sameIntegerInteger repeated
      classResolutionProofInstanceHead repeatedProof @?= sameRepeatedHead
      expectNoDischarge
        $ dischargeGroundConstraint environment sameIntegerBoolean
      applied <- expectDischarge
        $ dischargeGroundConstraint environment appliedFunction
      appliedProof <- expectReplay $ replayCheckedConstraintDischarge
        environment appliedFunction applied
      classResolutionProofInstanceHead appliedProof @?= appliedHigherHead
      expectNoDischarge
        $ dischargeGroundConstraint environment missingBinderInteger
  , testCase "canonicalize runtime substitutions before derived bounds" $ do
      inventory <- inventoryFrom $ runtimeCanonicalizationDeclarations ()
      limits <- expectRight
        $ mkClassResolutionLimits 7 2 2 3 4 3 3 16 2 2
      environment <- expectRight
        $ sealClassResolutionEnvironment limits inventory
      receipt <- expectDischarge
        $ dischargeGroundConstraint environment runtimeCanonicalizationGoal
      proof <- expectReplay $ replayCheckedConstraintDischarge
        environment runtimeCanonicalizationGoal receipt
      proofGoalsPreorder proof @?=
        [runtimeCanonicalizationGoal, runtimeCanonicalBaseGoal]
  , testCase "stop prerequisite instantiation at the first unusable binder" $ do
      inventory <- inventoryFrom $ shortCircuitDeclarations ()
      limits <- expectRight
        $ mkClassResolutionLimits 9 3 1 5 3 5 3 16 1 1
      environment <- expectRight
        $ sealClassResolutionEnvironment limits inventory
      expectNoDischarge
        $ dischargeGroundConstraint environment shortCircuitGoal
  , testCase "stop derived validation after unresolved evidence" $ do
      inventory <- inventoryFrom $ unresolvedPrefixDeclarations ()
      limits <- expectRight
        $ mkClassResolutionLimits 9 3 1 5 3 5 3 16 1 1
      environment <- expectRight
        $ sealClassResolutionEnvironment limits inventory
      expectNoDischarge
        $ dischargeGroundConstraint environment shortCircuitGoal
  , testCase "validate every raw ground query before resolution" $ do
      environment <- sealDeclarations defaultClassResolutionLimits
        $ queryDeclarations ()
      expectQueryError
        (InvalidClassResolutionGroundConstraint
          $ UnknownClassResolutionClass missingClassName)
        $ dischargeGroundConstraint environment
        $ Constraint missingClassName [integerType]
      expectQueryError
        (InvalidClassResolutionGroundConstraint
          $ ClassResolutionConstraintArityMismatch queryClassName 1 0)
        $ dischargeGroundConstraint environment
        $ Constraint queryClassName []
      expectQueryError
        (ClassResolutionGroundConstraintHasFreeVariables ["free"])
        $ dischargeGroundConstraint environment
        $ Constraint queryClassName [typeVariable "free"]
      expectQueryError
        (ClassResolutionGroundConstraintHasFreeVariables ["free"])
        $ dischargeGroundConstraint environment
        $ Constraint queryClassName
            [Type.TypeApplication (typeVariable "free")
              (typeVariable "free")]
      expectQueryError
        (InvalidClassResolutionGroundConstraint
          $ IllKindedClassResolutionConstraint
          $ KindInference.UnknownTypeConstructor unknownTypeName)
        $ dischargeGroundConstraint environment
        $ Constraint queryClassName [typeConstructor unknownTypeName]
      case dischargeGroundConstraint environment
          (Constraint queryClassName [typeConstructor maybeTypeName]) of
        Left (InvalidClassResolutionGroundConstraint
            (IllKindedClassResolutionConstraint KindInference.KindMismatch{})) ->
          pure ()
        _ -> assertFailure "a higher-kinded proper-type argument was accepted"
  , testCase "reject raw aliases and nested foralls explicitly" $ do
      aliasInventory <- inventoryFrom $ aliasInstanceDeclarations ()
      expectEnvironmentError
        (InvalidClassResolutionEnvironmentConstraint
          (ClassResolutionInstanceHead 0)
          (ClassResolutionConstraintContainsTypeSynonym 0 aliasTypeName))
        $ sealClassResolutionEnvironment
            defaultClassResolutionLimits aliasInventory
      environment <- sealDeclarations defaultClassResolutionLimits
        $ queryDeclarations ()
      expectQueryError
        (InvalidClassResolutionGroundConstraint
          $ ClassResolutionConstraintContainsTypeSynonym 0 aliasTypeName)
        $ dischargeGroundConstraint environment
        $ Constraint queryClassName [applyType aliasTypeName integerType]
      expectQueryError
        (InvalidClassResolutionGroundConstraint
          $ ClassResolutionConstraintForallUnsupported 0)
        $ dischargeGroundConstraint environment
        $ Constraint queryClassName
            [Type.ForallType ["bound"] [] $ typeVariable "bound"]
  , testCase "reject overlap, superclass cycles, and growing prerequisites" $ do
      overlapInventory <- inventoryFrom $ overlapDeclarations ()
      expectEnvironmentError
        (OverlappingClassResolutionInstanceHeads
          overlapGenericHead overlapIntegerHead)
        $ sealClassResolutionEnvironment
            defaultClassResolutionLimits overlapInventory
      higherOverlapInventory <- inventoryFrom
        $ higherKindedOverlapDeclarations ()
      expectEnvironmentError
        (OverlappingClassResolutionInstanceHeads
          appliedHigherHead appliedFunction)
        $ sealClassResolutionEnvironment
            defaultClassResolutionLimits higherOverlapInventory
      cycleInventory <- inventoryFrom $ superclassCycleDeclarations ()
      expectEnvironmentError
        (ClassResolutionSuperclassCycle [cycleLeftName, cycleRightName])
        $ sealClassResolutionEnvironment
            defaultClassResolutionLimits cycleInventory
      growingInventory <- inventoryFrom
        $ growingPrerequisiteDeclarations ()
      expectEnvironmentError
        (ExpandingClassResolutionInstancePrerequisite
          0 growingHead growingPrerequisite)
        $ sealClassResolutionEnvironment
            defaultClassResolutionLimits growingInventory
      superclassInventory <- inventoryFrom
        $ growingSuperclassDeclarations ()
      expectEnvironmentError
        (ExpandingClassResolutionInstancePrerequisite
          0 growingDerivedHead growingSuperclassPrerequisite)
        $ sealClassResolutionEnvironment
            defaultClassResolutionLimits superclassInventory
  , testCase "enforce environment, query, depth, and proof-node limits" $ do
      mkClassResolutionLimits (-1) 0 0 0 0 0 0 0 0 0 @?=
        Left (NegativeClassResolutionLimit ClassResolutionDeclarations (-1))
      recursiveInventory <- inventoryFrom $ recursiveDeclarations ()
      tooFewDeclarations <- expectRight
        $ mkClassResolutionLimits 3 1 2 8 4 8 4 16 3 3
      expectEnvironmentError
        (ClassResolutionDeclarationLimitExceeded 3 4)
        $ sealClassResolutionEnvironment
            tooFewDeclarations recursiveInventory
      noClasses <- expectRight
        $ mkClassResolutionLimits 4 0 2 8 4 8 4 16 3 3
      expectEnvironmentError
        (ClassResolutionClassLimitExceeded 0 1)
        $ sealClassResolutionEnvironment noClasses recursiveInventory
      oneInstance <- expectRight
        $ mkClassResolutionLimits 4 1 1 8 4 8 4 16 3 3
      expectEnvironmentError
        (ClassResolutionInstanceLimitExceeded 1 2)
        $ sealClassResolutionEnvironment oneInstance recursiveInventory
      exact <- expectRight
        $ mkClassResolutionLimits 4 1 2 8 4 8 4 16 3 3
      exactEnvironment <- expectRight
        $ sealClassResolutionEnvironment exact recursiveInventory
      _ <- expectDischarge
        $ dischargeGroundConstraint exactEnvironment recursiveNestedInteger
      shallow <- expectRight
        $ mkClassResolutionLimits 4 1 2 8 4 8 4 16 2 3
      shallowEnvironment <- expectRight
        $ sealClassResolutionEnvironment shallow recursiveInventory
      expectQueryError
        (ClassResolutionProofDepthLimitExceeded 2 3)
        $ dischargeGroundConstraint
            shallowEnvironment recursiveNestedInteger
      tooFewProofs <- expectRight
        $ mkClassResolutionLimits 4 1 2 8 4 8 4 16 3 2
      proofEnvironment <- expectRight
        $ sealClassResolutionEnvironment tooFewProofs recursiveInventory
      expectQueryError
        (ClassResolutionProofNodeLimitExceeded 2 3)
        $ dischargeGroundConstraint proofEnvironment recursiveNestedInteger
      simpleInventory <- inventoryFrom $ simpleDeclarations ()
      noConstructorKinds <- expectRight
        $ mkClassResolutionLimits 3 1 1 0 4 8 4 16 2 2
      expectEnvironmentError
        (ClassResolutionTypeConstructorKindLimitExceeded 0 1)
        $ sealClassResolutionEnvironment
            noConstructorKinds simpleInventory
      smallTypes <- expectRight
        $ mkClassResolutionLimits 3 1 1 8 4 2 4 16 2 2
      smallTypeEnvironment <- expectRight
        $ sealClassResolutionEnvironment smallTypes simpleInventory
      expectQueryError
        (InvalidClassResolutionGroundConstraint
          $ ClassResolutionConstraintTypeNodeLimitExceeded 0 2 3)
        $ dischargeGroundConstraint smallTypeEnvironment
        $ Constraint recursiveClassName [listType integerType]
      kindInventory <- inventoryFrom
        [ abstract () maybeTypeName
            $ Kind.FunctionKind Kind.ProperTypeKind Kind.ProperTypeKind
        ]
      smallKinds <- expectRight
        $ mkClassResolutionLimits 1 0 0 1 4 8 2 16 1 1
      expectEnvironmentError
        (ClassResolutionKindNodeLimitExceeded
          (ClassResolutionTypeConstructorKind maybeTypeName) 2 3)
        $ sealClassResolutionEnvironment smallKinds kindInventory
      overlapInventory <- inventoryFrom $ overlapDeclarations ()
      noOverlapComparisons <- expectRight
        $ mkClassResolutionLimits 4 1 2 1 1 1 1 0 1 1
      expectEnvironmentError
        (ClassResolutionOverlapComparisonLimitExceeded 0 1)
        $ sealClassResolutionEnvironment
            noOverlapComparisons overlapInventory
  , testCase "revalidate completed and instantiated prerequisites" $ do
      completionInventory <- inventoryFrom
        $ completedWidthDeclarations ()
      narrowCompletion <- expectRight
        $ mkClassResolutionLimits 5 4 1 0 2 3 1 16 1 1
      expectEnvironmentError
        (ClassResolutionCollectionLimitExceeded
          (ClassResolutionCompletedInstancePrerequisites 0) 2 3)
        $ sealClassResolutionEnvironment
            narrowCompletion completionInventory
      deduplicatedEnvironment <- sealDeclarations
        defaultClassResolutionLimits $ canonicalSuperclassDeclarations ()
      receipt <- expectDischarge $ dischargeGroundConstraint
        deduplicatedEnvironment canonicalDerivedGoal
      proof <- expectReplay $ replayCheckedConstraintDischarge
        deduplicatedEnvironment canonicalDerivedGoal receipt
      map classResolutionProofGoal
        (classResolutionProofPrerequisites proof) @?=
          [canonicalBaseGoal]
      derivedInventory <- inventoryFrom $ derivedLimitDeclarations ()
      derivedLimits <- expectRight
        $ mkClassResolutionLimits 7 2 1 4 2 5 3 16 2 2
      derivedEnvironment <- expectRight
        $ sealClassResolutionEnvironment derivedLimits derivedInventory
      expectQueryError
        (InvalidClassResolutionDerivedConstraint 0 0
          $ ClassResolutionConstraintTypeNodeLimitExceeded 0 5 6)
        $ dischargeGroundConstraint
            derivedEnvironment derivedLimitGoal
  , testCase "reject stale environments and different replay goals" $ do
      environment <- sealDeclarations defaultClassResolutionLimits
        $ successDeclarations ()
      receipt <- expectDischarge
        $ dischargeGroundConstraint environment derivedInteger
      _ <- expectReplay
        $ replayCheckedConstraintDischarge environment derivedInteger receipt
      expectReplayMismatch
        (ClassResolutionReplayGoalMismatch derivedInteger derivedBoolean)
        $ replayCheckedConstraintDischarge environment derivedBoolean receipt
      otherEnvironment <- sealDeclarations defaultClassResolutionLimits
        $ successDeclarations () ++ [classOne () extraClassName []]
      expectReplayMismatch ClassResolutionReplayEnvironmentMismatch
        $ replayCheckedConstraintDischarge
            otherEnvironment derivedInteger receipt
      case replayCheckedConstraintDischarge otherEnvironment
          (error "environment mismatch demanded the replay goal") receipt of
        Left ClassResolutionReplayEnvironmentMismatch -> pure ()
        _ -> assertFailure "replay inspected a goal before environment authority"
  , testCase "deep evaluation ignores every source annotation" $ do
      let poison :: Int
          poison = error "class resolution forced a source annotation"
      environment <- sealDeclarations defaultClassResolutionLimits
        $ successDeclarations poison
      receipt <- expectDischarge
        $ dischargeGroundConstraint environment derivedInteger
      _ <- evaluate $ force environment
      _ <- evaluate $ force receipt
      proof <- expectReplay
        $ replayCheckedConstraintDischarge environment derivedInteger receipt
      _ <- evaluate $ force proof
      pure ()
  ]

successDeclarations
  :: annotation
  -> [Declaration.Declaration String Void annotation]
successDeclarations annotation =
  [ abstract annotation integerTypeName Kind.ProperTypeKind
  , abstract annotation booleanTypeName Kind.ProperTypeKind
  , classOne annotation leftEvidenceName []
  , classOne annotation rightEvidenceName []
  , classOne annotation baseClassName []
  , classOne annotation derivedClassName
      [Constraint baseClassName [typeVariable "a"]]
  , groundInstance annotation leftInteger
  , groundInstance annotation rightInteger
  , groundInstance annotation baseInteger
  , Declaration.InstanceDeclaration annotation ["a"]
      [ Constraint leftEvidenceName [typeVariable "a"]
      , Constraint rightEvidenceName [typeVariable "a"]
      ] derivedVariable
  ]

recursiveDeclarations
  :: annotation
  -> [Declaration.Declaration String Void annotation]
recursiveDeclarations annotation =
  [ abstract annotation integerTypeName Kind.ProperTypeKind
  , classOne annotation recursiveClassName []
  , groundInstance annotation recursiveInteger
  , Declaration.InstanceDeclaration annotation ["a"]
      [Constraint recursiveClassName [typeVariable "a"]]
      (Constraint recursiveClassName [listType $ typeVariable "a"])
  ]

cyclicDeclarations
  :: annotation
  -> [Declaration.Declaration String Void annotation]
cyclicDeclarations annotation =
  [ abstract annotation integerTypeName Kind.ProperTypeKind
  , classOne annotation recursiveClassName []
  , Declaration.InstanceDeclaration annotation ["a"]
      [Constraint recursiveClassName [typeVariable "a"]]
      (Constraint recursiveClassName [typeVariable "a"])
  ]

matcherDeclarations
  :: annotation
  -> [Declaration.Declaration String Void annotation]
matcherDeclarations annotation =
  [ abstract annotation integerTypeName Kind.ProperTypeKind
  , abstract annotation booleanTypeName Kind.ProperTypeKind
  , Declaration.ClassDeclaration annotation sameClassName
      [properParameter "left", properParameter "right"] [] []
  , classOne annotation appliedClassName []
  , classOne annotation missingBinderClassName []
  , classOne annotation neededClassName []
  , Declaration.InstanceDeclaration annotation ["a"] [] sameRepeatedHead
  , Declaration.InstanceDeclaration annotation ["f", "a", "b"] []
      appliedHigherHead
  , Declaration.InstanceDeclaration annotation ["a", "unused"]
      [Constraint neededClassName [typeVariable "unused"]]
      (Constraint missingBinderClassName [typeVariable "a"])
  ]

runtimeCanonicalizationDeclarations
  :: annotation
  -> [Declaration.Declaration String Void annotation]
runtimeCanonicalizationDeclarations annotation =
  [ abstract annotation integerTypeName Kind.ProperTypeKind
  , abstract annotation booleanTypeName Kind.ProperTypeKind
  , abstract annotation markerTypeName Kind.ProperTypeKind
  , classOne annotation baseClassName []
  , Declaration.ClassDeclaration annotation runtimeClassName
      [ kindedParameter "f"
          $ Kind.FunctionKind Kind.ProperTypeKind Kind.ProperTypeKind
      , properParameter "a"
      , properParameter "marker"
      ] [] []
  , groundInstance annotation runtimeCanonicalBaseGoal
  , Declaration.InstanceDeclaration annotation ["f", "a", "marker"]
      [Constraint baseClassName
        [Type.TypeApplication (typeVariable "f") (typeVariable "a")]]
      (Constraint runtimeClassName
        [ typeVariable "f"
        , typeVariable "a"
        , typeVariable "marker"
        ])
  ]

shortCircuitDeclarations
  :: annotation
  -> [Declaration.Declaration String Void annotation]
shortCircuitDeclarations annotation = shortCircuitDeclarationsWith
  annotation ["a", "b", "missing"] $ typeVariable "missing"

unresolvedPrefixDeclarations
  :: annotation
  -> [Declaration.Declaration String Void annotation]
unresolvedPrefixDeclarations annotation = shortCircuitDeclarationsWith
  annotation ["a", "b"] $ typeVariable "a"

shortCircuitDeclarationsWith
  :: annotation
  -> [String]
  -> Type.Type String
  -> [Declaration.Declaration String Void annotation]
shortCircuitDeclarationsWith annotation binders unavailableArgument =
  [ abstract annotation integerTypeName Kind.ProperTypeKind
  , abstract annotation booleanTypeName Kind.ProperTypeKind
  , abstract annotation maybeTypeName unaryProperKind
  , abstract annotation shortFunctionLeftName unaryProperKind
  , abstract annotation shortFunctionRightName unaryProperKind
  , classOne annotation unavailableClassName []
  , classOne annotation latePrerequisiteClassName []
  , Declaration.ClassDeclaration annotation shortCircuitClassName
      [properParameter "left", properParameter "right"] [] []
  , Declaration.InstanceDeclaration annotation binders
      [ Constraint unavailableClassName [unavailableArgument]
      , Constraint latePrerequisiteClassName
          [Type.TupleType Boxed [typeVariable "a", typeVariable "b"]]
      ]
      (Constraint shortCircuitClassName
        [ applyType shortFunctionLeftName $ typeVariable "a"
        , applyType shortFunctionRightName $ typeVariable "b"
        ])
  ]
 where
  unaryProperKind =
    Kind.FunctionKind Kind.ProperTypeKind Kind.ProperTypeKind

queryDeclarations
  :: annotation
  -> [Declaration.Declaration String Void annotation]
queryDeclarations annotation =
  [ abstract annotation integerTypeName Kind.ProperTypeKind
  , abstract annotation maybeTypeName
      $ Kind.FunctionKind Kind.ProperTypeKind Kind.ProperTypeKind
  , Declaration.TypeSynonymDeclaration annotation aliasTypeName
      [ordinaryParameter "a"] $ typeVariable "a"
  , classOne annotation queryClassName []
  , groundInstance annotation
      $ Constraint queryClassName [integerType]
  ]

aliasInstanceDeclarations
  :: annotation
  -> [Declaration.Declaration String Void annotation]
aliasInstanceDeclarations annotation =
  [ abstract annotation integerTypeName Kind.ProperTypeKind
  , Declaration.TypeSynonymDeclaration annotation aliasTypeName
      [ordinaryParameter "a"] $ typeVariable "a"
  , classOne annotation queryClassName []
  , groundInstance annotation
      $ Constraint queryClassName [applyType aliasTypeName integerType]
  ]

overlapDeclarations
  :: annotation
  -> [Declaration.Declaration String Void annotation]
overlapDeclarations annotation =
  [ abstract annotation integerTypeName Kind.ProperTypeKind
  , classOne annotation overlapClassName []
  , Declaration.InstanceDeclaration annotation ["a"] [] overlapGenericHead
  , groundInstance annotation overlapIntegerHead
  ]

higherKindedOverlapDeclarations
  :: annotation
  -> [Declaration.Declaration String Void annotation]
higherKindedOverlapDeclarations annotation =
  [ abstract annotation integerTypeName Kind.ProperTypeKind
  , abstract annotation booleanTypeName Kind.ProperTypeKind
  , classOne annotation appliedClassName []
  , Declaration.InstanceDeclaration annotation ["f", "a", "b"] []
      appliedHigherHead
  , groundInstance annotation appliedFunction
  ]

superclassCycleDeclarations
  :: annotation
  -> [Declaration.Declaration String Void annotation]
superclassCycleDeclarations annotation =
  [ Declaration.ClassDeclaration annotation cycleLeftName
      [properParameter "a"]
      [Constraint cycleRightName [typeVariable "a"]] []
  , Declaration.ClassDeclaration annotation cycleRightName
      [properParameter "a"]
      [Constraint cycleLeftName [typeVariable "a"]] []
  ]

growingPrerequisiteDeclarations
  :: annotation
  -> [Declaration.Declaration String Void annotation]
growingPrerequisiteDeclarations annotation =
  [ classOne annotation growingClassName []
  , Declaration.InstanceDeclaration annotation ["a"]
      [growingPrerequisite] growingHead
  ]

growingSuperclassDeclarations
  :: annotation
  -> [Declaration.Declaration String Void annotation]
growingSuperclassDeclarations annotation =
  [ classOne annotation growingBaseName []
  , Declaration.ClassDeclaration annotation growingDerivedName
      [properParameter "a"] [growingSuperclassPrerequisite] []
  , Declaration.InstanceDeclaration annotation ["a"] [] growingDerivedHead
  ]

completedWidthDeclarations
  :: annotation
  -> [Declaration.Declaration String Void annotation]
completedWidthDeclarations annotation =
  [ classOne annotation completionExtraName []
  , classOne annotation completionLeftName []
  , classOne annotation completionRightName []
  , classOne annotation completionOwnerName
      [ Constraint completionLeftName [typeVariable "a"]
      , Constraint completionRightName [typeVariable "a"]
      ]
  , Declaration.InstanceDeclaration annotation ["a"]
      [Constraint completionExtraName [typeVariable "a"]]
      (Constraint completionOwnerName [typeVariable "a"])
  ]

canonicalSuperclassDeclarations
  :: annotation
  -> [Declaration.Declaration String Void annotation]
canonicalSuperclassDeclarations annotation =
  [ abstract annotation integerTypeName Kind.ProperTypeKind
  , abstract annotation booleanTypeName Kind.ProperTypeKind
  , classOne annotation canonicalBaseName []
  , Declaration.ClassDeclaration annotation canonicalDerivedName
      [ kindedParameter "f"
          $ Kind.FunctionKind Kind.ProperTypeKind Kind.ProperTypeKind
      , properParameter "a"
      , properParameter "marker"
      ]
      [Constraint canonicalBaseName
        [Type.TypeApplication (typeVariable "f") (typeVariable "a")]]
      []
  , groundInstance annotation canonicalBaseGoal
  , Declaration.InstanceDeclaration annotation [] [canonicalBaseGoal]
      canonicalDerivedGoal
  ]

derivedLimitDeclarations
  :: annotation
  -> [Declaration.Declaration String Void annotation]
derivedLimitDeclarations annotation =
  [ abstract annotation integerTypeName Kind.ProperTypeKind
  , abstract annotation booleanTypeName Kind.ProperTypeKind
  , abstract annotation derivedFunctionLeftName
      $ Kind.FunctionKind Kind.ProperTypeKind Kind.ProperTypeKind
  , abstract annotation derivedFunctionRightName
      $ Kind.FunctionKind Kind.ProperTypeKind Kind.ProperTypeKind
  , Declaration.ClassDeclaration annotation derivedLimitOwnerName
      [properParameter "left", properParameter "right"] [] []
  , classOne annotation derivedLimitPrerequisiteName []
  , Declaration.InstanceDeclaration annotation ["a", "b"]
      [Constraint derivedLimitPrerequisiteName
        [Type.TupleType Boxed [typeVariable "a", typeVariable "b"]]]
      (Constraint derivedLimitOwnerName
        [ applyType derivedFunctionLeftName $ typeVariable "a"
        , applyType derivedFunctionRightName $ typeVariable "b"
        ])
  ]

simpleDeclarations
  :: annotation
  -> [Declaration.Declaration String Void annotation]
simpleDeclarations annotation =
  [ abstract annotation integerTypeName Kind.ProperTypeKind
  , classOne annotation recursiveClassName []
  , groundInstance annotation recursiveInteger
  ]

abstract
  :: annotation
  -> Name
  -> Kind.Kind Void
  -> Declaration.Declaration String Void annotation
abstract = Declaration.AbstractTypeDeclaration

classOne
  :: annotation
  -> Name
  -> [Constraint (Type.Type String)]
  -> Declaration.Declaration String Void annotation
classOne annotation name superclasses = Declaration.ClassDeclaration
  annotation name [properParameter "a"] superclasses []

groundInstance
  :: annotation
  -> Constraint (Type.Type String)
  -> Declaration.Declaration String Void annotation
groundInstance annotation = Declaration.InstanceDeclaration annotation [] []

properParameter :: String -> Declaration.TypeParameter String Void
properParameter variable = Declaration.TypeParameter variable
  $ Just Kind.ProperTypeKind

kindedParameter
  :: String
  -> Kind.Kind Void
  -> Declaration.TypeParameter String Void
kindedParameter variable kind = Declaration.TypeParameter variable $ Just kind

ordinaryParameter :: String -> Declaration.TypeParameter String Void
ordinaryParameter variable = Declaration.TypeParameter variable Nothing

typeVariable :: String -> Type.Type String
typeVariable = Type.TypeVariable

typeConstructor :: Name -> Type.Type String
typeConstructor = Type.TypeConstructor

applyType :: Name -> Type.Type String -> Type.Type String
applyType name = Type.TypeApplication $ typeConstructor name

listType :: Type.Type String -> Type.Type String
listType = applyType listName

integerType, booleanType :: Type.Type String
integerType = typeConstructor integerTypeName
booleanType = typeConstructor booleanTypeName

leftInteger, rightInteger, baseInteger, derivedInteger, derivedBoolean
  :: Constraint (Type.Type String)
leftInteger = Constraint leftEvidenceName [integerType]
rightInteger = Constraint rightEvidenceName [integerType]
baseInteger = Constraint baseClassName [integerType]
derivedInteger = Constraint derivedClassName [integerType]
derivedBoolean = Constraint derivedClassName [booleanType]

derivedVariable :: Constraint (Type.Type String)
derivedVariable = Constraint derivedClassName [typeVariable "a"]

recursiveInteger, recursiveListInteger, recursiveNestedInteger
  :: Constraint (Type.Type String)
recursiveInteger = Constraint recursiveClassName [integerType]
recursiveListInteger = Constraint recursiveClassName [listType integerType]
recursiveNestedInteger = Constraint recursiveClassName
  [listType $ listType integerType]

sameRepeatedHead, sameIntegerInteger, sameIntegerBoolean
  :: Constraint (Type.Type String)
sameRepeatedHead = Constraint sameClassName
  [typeVariable "a", typeVariable "a"]
sameIntegerInteger = Constraint sameClassName [integerType, integerType]
sameIntegerBoolean = Constraint sameClassName [integerType, booleanType]

appliedHigherHead, appliedFunction :: Constraint (Type.Type String)
appliedHigherHead = Constraint appliedClassName
  [ Type.TypeApplication
      (Type.TypeApplication (typeVariable "f") $ typeVariable "a")
      (typeVariable "b")
  ]
appliedFunction = Constraint appliedClassName
  [Type.FunctionType integerType booleanType]

missingBinderInteger :: Constraint (Type.Type String)
missingBinderInteger = Constraint missingBinderClassName [integerType]

runtimeCanonicalBaseGoal, runtimeCanonicalizationGoal
  :: Constraint (Type.Type String)
runtimeCanonicalBaseGoal = Constraint baseClassName
  [Type.FunctionType integerType booleanType]
runtimeCanonicalizationGoal = Constraint runtimeClassName
  [ Type.TypeApplication (typeConstructor functionName) integerType
  , booleanType
  , typeConstructor markerTypeName
  ]

shortCircuitGoal :: Constraint (Type.Type String)
shortCircuitGoal = Constraint shortCircuitClassName
  [ applyType shortFunctionLeftName $ applyType maybeTypeName integerType
  , applyType shortFunctionRightName $ applyType maybeTypeName booleanType
  ]

overlapGenericHead, overlapIntegerHead :: Constraint (Type.Type String)
overlapGenericHead = Constraint overlapClassName [typeVariable "a"]
overlapIntegerHead = Constraint overlapClassName [integerType]

growingHead, growingPrerequisite :: Constraint (Type.Type String)
growingHead = Constraint growingClassName [typeVariable "a"]
growingPrerequisite = Constraint growingClassName
  [listType $ typeVariable "a"]

growingDerivedHead, growingSuperclassPrerequisite
  :: Constraint (Type.Type String)
growingDerivedHead = Constraint growingDerivedName [typeVariable "a"]
growingSuperclassPrerequisite = Constraint growingBaseName
  [listType $ typeVariable "a"]

canonicalBaseGoal, canonicalDerivedGoal :: Constraint (Type.Type String)
canonicalBaseGoal = Constraint canonicalBaseName
  [Type.FunctionType integerType booleanType]
canonicalDerivedGoal = Constraint canonicalDerivedName
  [ Type.TypeApplication (Type.TypeConstructor functionName) integerType
  , booleanType
  , integerType
  ]

derivedLimitGoal :: Constraint (Type.Type String)
derivedLimitGoal = Constraint derivedLimitOwnerName
  [ applyType derivedFunctionLeftName $ listType integerType
  , applyType derivedFunctionRightName $ listType booleanType
  ]

integerTypeName, booleanTypeName, maybeTypeName, aliasTypeName :: Name
integerTypeName = fixtureName "Int"
booleanTypeName = fixtureName "Bool"
maybeTypeName = fixtureName "Maybe"
aliasTypeName = fixtureName "Alias"

leftEvidenceName, rightEvidenceName, baseClassName, derivedClassName :: Name
leftEvidenceName = fixtureName "LeftEvidence"
rightEvidenceName = fixtureName "RightEvidence"
baseClassName = fixtureName "Base"
derivedClassName = fixtureName "Derived"

recursiveClassName, sameClassName, appliedClassName :: Name
recursiveClassName = fixtureName "Recursive"
sameClassName = fixtureName "Same"
appliedClassName = fixtureName "Applied"

missingBinderClassName, neededClassName, queryClassName :: Name
missingBinderClassName = fixtureName "MissingBinder"
neededClassName = fixtureName "Needed"
queryClassName = fixtureName "QueryClass"

markerTypeName, runtimeClassName :: Name
markerTypeName = fixtureName "Marker"
runtimeClassName = fixtureName "Runtime"

shortFunctionLeftName, shortFunctionRightName, unavailableClassName,
    latePrerequisiteClassName, shortCircuitClassName :: Name
shortFunctionLeftName = fixtureName "ShortF"
shortFunctionRightName = fixtureName "ShortG"
unavailableClassName = fixtureName "Unavailable"
latePrerequisiteClassName = fixtureName "LatePrerequisite"
shortCircuitClassName = fixtureName "ShortCircuit"

missingClassName, unknownTypeName, overlapClassName :: Name
missingClassName = fixtureName "MissingClass"
unknownTypeName = fixtureName "UnknownType"
overlapClassName = fixtureName "Overlap"

cycleLeftName, cycleRightName, growingClassName :: Name
cycleLeftName = fixtureName "CycleLeft"
cycleRightName = fixtureName "CycleRight"
growingClassName = fixtureName "Growing"

growingBaseName, growingDerivedName, extraClassName :: Name
growingBaseName = fixtureName "GrowingBase"
growingDerivedName = fixtureName "GrowingDerived"
extraClassName = fixtureName "Extra"

completionExtraName, completionLeftName, completionRightName,
    completionOwnerName :: Name
completionExtraName = fixtureName "CompletionExtra"
completionLeftName = fixtureName "CompletionLeft"
completionRightName = fixtureName "CompletionRight"
completionOwnerName = fixtureName "CompletionOwner"

canonicalBaseName, canonicalDerivedName :: Name
canonicalBaseName = fixtureName "CanonicalBase"
canonicalDerivedName = fixtureName "CanonicalDerived"

derivedFunctionLeftName, derivedFunctionRightName,
    derivedLimitOwnerName, derivedLimitPrerequisiteName :: Name
derivedFunctionLeftName = fixtureName "DerivedF"
derivedFunctionRightName = fixtureName "DerivedG"
derivedLimitOwnerName = fixtureName "DerivedLimitOwner"
derivedLimitPrerequisiteName = fixtureName "DerivedLimitPrerequisite"

fixtureName :: String -> Name
fixtureName source = case mkIdentifier source of
  Left failure -> error $ "invalid class-resolution fixture name: "
    ++ show failure
  Right name -> name

inventoryFrom
  :: [Declaration.Declaration String Void annotation]
  -> IO (Inventory.Inventory String annotation)
inventoryFrom declarations = expectRight
  $ Inventory.mkInventory KindInference.ClosedKindInventory declarations

sealDeclarations
  :: ClassResolutionLimits
  -> [Declaration.Declaration String Void annotation]
  -> IO (CheckedClassResolutionEnvironment String)
sealDeclarations limits declarations = do
  inventory <- inventoryFrom declarations
  expectRight $ sealClassResolutionEnvironment limits inventory

proofGoalsPreorder
  :: ClassResolutionProof variable
  -> [Constraint (Type.Type variable)]
proofGoalsPreorder proof = classResolutionProofGoal proof
  : concatMap proofGoalsPreorder (classResolutionProofPrerequisites proof)

expectRight :: Show failure => Either failure value -> IO value
expectRight result = case result of
  Left failure -> assertFailure (show failure) >> error "unreachable"
  Right value -> pure value

expectDischarge
  :: Show variable
  => Either (ClassResolutionQueryError variable)
      (Maybe (CheckedConstraintDischarge variable))
  -> IO (CheckedConstraintDischarge variable)
expectDischarge result = case result of
  Left failure -> assertFailure (show failure) >> error "unreachable"
  Right Nothing -> assertFailure "expected class evidence" >> error "unreachable"
  Right (Just receipt) -> pure receipt

expectNoDischarge
  :: Show variable
  => Either (ClassResolutionQueryError variable)
      (Maybe (CheckedConstraintDischarge variable))
  -> IO ()
expectNoDischarge result = case result of
  Left failure -> assertFailure $ show failure
  Right Nothing -> pure ()
  Right Just{} -> assertFailure "unexpected class evidence"

expectEnvironmentError
  :: (Eq variable, Show variable)
  => ClassResolutionEnvironmentError variable
  -> Either (ClassResolutionEnvironmentError variable)
      (CheckedClassResolutionEnvironment variable)
  -> IO ()
expectEnvironmentError expected result = case result of
  Left actual -> actual @?= expected
  Right _ -> assertFailure "expected class-resolution environment rejection"

expectQueryError
  :: (Eq variable, Show variable)
  => ClassResolutionQueryError variable
  -> Either (ClassResolutionQueryError variable)
      (Maybe (CheckedConstraintDischarge variable))
  -> IO ()
expectQueryError expected result = case result of
  Left actual -> actual @?= expected
  Right _ -> assertFailure "expected class-resolution query rejection"

expectReplay
  :: Show variable
  => Either (ClassResolutionReplayMismatch variable)
      (ClassResolutionProof variable)
  -> IO (ClassResolutionProof variable)
expectReplay result = case result of
  Left failure -> assertFailure (show failure) >> error "unreachable"
  Right proof -> pure proof

expectReplayMismatch
  :: (Eq variable, Show variable)
  => ClassResolutionReplayMismatch variable
  -> Either (ClassResolutionReplayMismatch variable)
      (ClassResolutionProof variable)
  -> IO ()
expectReplayMismatch expected result = case result of
  Left actual -> actual @?= expected
  Right _ -> assertFailure "expected class-resolution replay rejection"
