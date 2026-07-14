module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Language.Haskell.Synthesis.Candidate
import Language.Haskell.Synthesis.Constraint
import Language.Haskell.Synthesis.Diagnostic
import qualified Language.Haskell.Synthesis.Declaration as Declaration
import qualified Language.Haskell.Synthesis.Environment as Environment
import Language.Haskell.Synthesis.Generated
import Language.Haskell.Synthesis.Name
import qualified Language.Haskell.Synthesis.Kind as Kind
import qualified Language.Haskell.Synthesis.KindInference as KindInference
import qualified Language.Haskell.Synthesis.Inventory as Inventory
import Language.Haskell.Synthesis.Query
import Language.Haskell.Synthesis.Search
import Language.Haskell.Synthesis.Selection
import qualified Language.Haskell.Synthesis.TypeRender as TypeRender
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeSynonym as TypeSynonym
import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Djex synthesis foundation"
  [ candidateTests
  , constraintTests
  , declarationTests
  , environmentTests
  , generatedTests
  , queryTests
  , searchTests
  , selectionTests
  , typeTests
  , synonymTests
  , kindInferenceTests
  , moduleTests
  , ordinaryTests
  , specialTests
  , parserTests
  , diagnosticTests
  , localOption (QC.QuickCheckTests 1000) propertyTests
  ]

candidateTests :: TestTree
candidateTests = testGroup "candidates"
  [ testCase "retain output, residual constraints, and backend details" $ do
      let equality = right $ mkIdentifier "Eq"
          candidate :: Candidate String [Int] Int
          candidate = Candidate
            { candidateOutput = 7
            , candidateResidualConstraints = [Constraint equality ["a"]]
            , candidateDetails = [10, 20]
            }
      candidateOutput candidate @?= 7
      candidateResidualConstraints candidate @?=
        [Constraint equality ["a"]]
      candidateDetails candidate @?= [10, 20]
      show candidate @?=
        "Candidate {candidateOutput = 7, candidateResidualConstraints = \
        \[Eq \"a\"], candidateDetails = [10,20]}"
  , testCase "map, fold, and traverse affect only generated output" $ do
      let equality = right $ mkIdentifier "Eq"
          candidate :: Candidate String String Int
          candidate = Candidate 3 [Constraint equality ["a"]] "details"
          expected output = Candidate output
            [Constraint equality ["a"]] "details"
      fmap (+ 4) candidate @?= expected 7
      sum candidate @?= 3
      traverse (Just . show) candidate @?= Just (expected "3")
  , testCase "deep evaluation reaches every candidate component" $ do
      let equality = right $ mkIdentifier "Eq"
          candidate :: Candidate [Int] [Int] [Int]
          candidate = Candidate [1, 2]
            [Constraint equality [[3, 4]]] [5, 6]
      _ <- evaluate $ force candidate
      pure ()
  , testCase "render a clause candidate as an expression or definition" $ do
      let identity = right $ mkIdentifier "identity"
          checkedIdentity = right $ mkDefinitionName identity
          candidate :: Candidate String () (FunctionClause Int)
          candidate = Candidate
            (FunctionClause checkedIdentity [Bind 0] $ Local 0) [] ()
          options = defaultRenderOptions $ \local -> "a" ++ show local
      renderCandidateExpression options candidate @?= Right "\\a0 -> a0"
      renderCandidateDefinition options candidate @?=
        Right "identity a0 = a0"
  , testCase "return shared syntax errors from candidate rendering" $ do
      let target = right $ mkIdentifier "result"
          checkedTarget = right $ mkDefinitionName target
          global = right $ parseName "Fixture.result"
          candidate :: Candidate String () (FunctionClause Int)
          candidate = Candidate
            (FunctionClause checkedTarget [] $ Global global) [] ()
          options = (defaultRenderOptions $ const "a")
            { renderQualification = Unqualified }
      renderCandidateDefinition options candidate @?=
        Left (GlobalDefinitionCapture target global Unqualified)
  ]

queryTests :: TestTree
queryTests = testGroup "queries"
  [ testCase "leave a context-free shared goal unchanged" $ do
      let goal = variable "a"
      requestContextualType (request goal []) @?= goal
  , testCase "quantify contexts around an unquantified goal" $ do
      let goal = SharedType.FunctionType (variable "a") (variable "b")
          contexts = [constraint "Eq" "a"]
      requestContextualType (request goal contexts) @?=
        SharedType.ForallType [] contexts goal
  , testCase "prepend contexts at one leading forall" $ do
      let embedded = constraint "Embedded" "a"
          explicit = constraint "Requested" "a"
          body = variable "a"
          goal = SharedType.ForallType ["a"] [embedded] body
      requestContextualType (request goal [explicit]) @?=
        SharedType.ForallType ["a"] [explicit, embedded] body
  , testCase "insert contexts beneath every leading forall" $ do
      let outer = constraint "Outer" "a"
          inner = constraint "Inner" "b"
          explicit = constraint "Requested" "a"
          goal = SharedType.ForallType ["a"] [outer]
            $ SharedType.ForallType ["b"] [inner]
            $ SharedType.FunctionType (variable "a") (variable "b")
      requestContextualType (request goal [explicit]) @?=
        SharedType.ForallType ["a"] [outer]
          (SharedType.ForallType ["b"] [explicit, inner]
            $ SharedType.FunctionType (variable "a") (variable "b"))
  , testCase "do not cross a non-leading type boundary" $ do
      let nested = SharedType.ForallType ["b"] [] $ variable "b"
          goal = SharedType.FunctionType (variable "a") nested
          explicit = constraint "Requested" "a"
      requestContextualType (request goal [explicit]) @?=
        SharedType.ForallType [] [explicit] goal
  ]
 where
  request goal contexts = QueryRequest
    { requestTarget = right $ mkDefinitionName $ right $ mkIdentifier "result"
    , requestGoal = goal
    , requestContexts = contexts
    , requestOptions = ()
    }
  variable = SharedType.TypeVariable
  constraint classSpelling variableSpelling = Constraint
    (right $ mkIdentifier classSpelling)
    [variable variableSpelling]

kindInferenceTests :: TestTree
kindInferenceTests = testGroup "kind inference"
  [ testCase "infer higher-kinded variables shared across types" $ do
      let maybeName = right $ mkIdentifier "Maybe"
          intName = right $ mkIdentifier "Int"
          assumptions = KindInference.KindAssumptions
            (Map.fromList
              [ (maybeName, arrow proper proper)
              , (intName, proper)
              ])
            Map.empty
          application = SharedType.TypeApplication
            (SharedType.TypeVariable "f")
            (SharedType.TypeConstructor intName)
      KindInference.inferSharedVariableKinds assumptions ["f"]
          [application] @?=
        Right [("f", arrow proper proper)]
  , testCase "share free-variable kinds across obligations" $ do
      let intName = right $ mkIdentifier "Int"
          assumptions = KindInference.emptyKindAssumptions
            { KindInference.typeConstructorKinds =
                Map.singleton intName proper
            }
          variable = SharedType.TypeVariable "f"
          application = SharedType.TypeApplication variable
            $ SharedType.TypeConstructor intName
      KindInference.checkTypesKinds assumptions
          [(proper, variable), (proper, application)] @?=
        Left (KindInference.KindMismatch proper $ arrow proper proper)
  , testCase "use declared class parameter kinds in forall contexts" $ do
      let functorName = right $ mkIdentifier "Functor"
          assumptions = KindInference.emptyKindAssumptions
            { KindInference.classParameterKinds =
                Map.singleton functorName [Just $ arrow proper proper]
            }
          quantified = SharedType.ForallType ["f"]
            [Constraint functorName [SharedType.TypeVariable "f"]]
            (SharedType.TypeVariable "f")
      KindInference.checkTypesKinds assumptions
        [(arrow proper proper, quantified)] @?= Right ()
      let polymorphic = assumptions
            { KindInference.classParameterKinds =
                Map.singleton functorName [Nothing]
            }
      KindInference.checkTypesKinds polymorphic
        [(arrow proper proper, quantified)] @?= Right ()
  , testCase "diagnose unknown classes and constraint arity" $ do
      let className = right $ mkIdentifier "C"
          variable = SharedType.TypeVariable "a"
          constrained arguments = SharedType.ForallType ["a"]
            [Constraint className arguments] variable
      KindInference.checkTypesKinds KindInference.emptyKindAssumptions
          [(proper, constrained [variable])] @?=
        Left (KindInference.UnknownClass className)
      let assumptions = KindInference.emptyKindAssumptions
            { KindInference.classParameterKinds =
                Map.singleton className []
            }
      KindInference.checkTypesKinds assumptions
          [(proper, constrained [variable])] @?=
        Left (KindInference.ClassArityMismatch className 0 1)
  , testCase "reject infinite kinds and accept intrinsic constructors" $ do
      let variable = SharedType.TypeVariable "a"
          selfApplication = SharedType.TypeApplication variable variable
          listApplication = SharedType.TypeApplication
            (SharedType.TypeConstructor listName) variable
      KindInference.checkTypesKinds KindInference.emptyKindAssumptions
          [(proper, selfApplication)] @?= Left KindInference.InfiniteKind
      KindInference.checkTypesKinds KindInference.emptyKindAssumptions
          [(proper, listApplication)] @?= Right ()
  , testCase "validate source types before kind inference" $ do
      let malformed = SharedType.TupleType Boxed
            [SharedType.TypeVariable (0 :: Int)]
          invalidClassName = right $ mkIdentifier "constraint"
          malformedConstraint :: SharedType.Type Int
          malformedConstraint = SharedType.ForallType [0]
            [Constraint invalidClassName [SharedType.TypeVariable 0]]
            (SharedType.TypeVariable 0)
      KindInference.checkTypesKinds KindInference.emptyKindAssumptions
          [(proper, malformed)] @?= Left
        (KindInference.InvalidKindInferenceType
          $ SharedType.InvalidTupleTypeArity Boxed 1)
      KindInference.checkTypesKinds KindInference.emptyKindAssumptions
          [(proper, malformedConstraint)] @?= Left
        (KindInference.InvalidKindInferenceType
          $ SharedType.InvalidTypeConstraint
          $ InvalidConstraintClass invalidClassName)
  , testCase "infer type constructors in dependency order" $ do
      let intName = right $ mkIdentifier "Int"
          fooName = right $ mkIdentifier "Foo"
          body = SharedType.TypeApplication
            (SharedType.TypeVariable "f")
            (SharedType.TypeVariable "a")
          declarations =
            [ KindInference.InferredTypeKind fooName ["f", "a"] [body]
            , KindInference.DeclaredTypeKind intName proper
            ]
      KindInference.inferAcyclicTypeConstructorKinds declarations @?=
        Right (Map.fromList
          [ (fooName, arrow (arrow proper proper) $ arrow proper proper)
          , (intName, proper)
          ])
  , testCase "reject duplicate, recursive, and unknown type declarations" $ do
      let aName = right $ mkIdentifier "A"
          bName = right $ mkIdentifier "B"
          declared :: Name -> KindInference.TypeKindDeclaration String
          declared name = KindInference.DeclaredTypeKind name proper
          refers :: Name -> Name -> KindInference.TypeKindDeclaration String
          refers name reference = KindInference.InferredTypeKind name []
            [SharedType.TypeConstructor reference]
      KindInference.inferAcyclicTypeConstructorKinds
          [declared aName, declared aName] @?=
        Left (KindInference.DuplicateTypeConstructor aName)
      KindInference.inferAcyclicTypeConstructorKinds
          [refers aName bName, refers bName aName] @?=
        Left (KindInference.RecursiveTypeDeclarations [aName, bName])
      KindInference.inferAcyclicTypeConstructorKinds
          [refers aName bName] @?=
        Left (KindInference.UnknownTypeConstructor bName)
  , testCase "infer a whole inventory with recursive higher-kinded data" $ do
      let fixName = right $ mkIdentifier "Fix"
          makeFixName = right $ mkIdentifier "MakeFix"
          className = right $ mkIdentifier "FunctorLike"
          methodName = right $ mkIdentifier "extract"
          parameter parameterName =
            Declaration.TypeParameter parameterName Nothing
          application function argument = SharedType.TypeApplication
            function argument
          fixOfF = application (SharedType.TypeConstructor fixName)
            (SharedType.TypeVariable "f")
          declarations :: [Declaration.Declaration String Void ()]
          declarations =
            [ Declaration.DataTypeDeclaration () fixName [parameter "f"]
                [ Declaration.DataConstructor () makeFixName
                    [application (SharedType.TypeVariable "f") fixOfF]
                ]
            , Declaration.ClassDeclaration () className [parameter "f"] []
                [ Declaration.ValueSignature () methodName
                    $ SharedType.FunctionType
                        (application (SharedType.TypeVariable "f")
                          $ SharedType.TypeVariable "a")
                        (SharedType.TypeVariable "a")
                ]
            ]
      KindInference.inferDeclarationKinds
          (sealedEnvironment declarations) @?= Right
        (KindInference.KindAssumptions
          (Map.singleton fixName $ arrow (arrow proper proper) proper)
          (Map.singleton className [Just $ arrow proper proper]))
  , testCase "operational uses cannot refine phantom parameter kinds" $ do
      let phantomName = right $ mkIdentifier "Phantom"
          higherName = right $ mkIdentifier "Higher"
          makeHigherName = right $ mkIdentifier "MakeHigher"
          intName = right $ mkIdentifier "Int"
          badName = right $ mkIdentifier "bad"
          parameter variable = Declaration.TypeParameter variable Nothing
          declarations :: [Declaration.Declaration String Void ()]
          declarations =
            [ Declaration.AbstractTypeDeclaration () intName proper
            , Declaration.TypeSynonymDeclaration () phantomName
                [parameter "unused"] $ SharedType.TypeConstructor intName
            , Declaration.DataTypeDeclaration () higherName [parameter "a"]
                [ Declaration.DataConstructor () makeHigherName
                    [SharedType.TypeVariable "a"]
                ]
            , Declaration.ValueDeclaration
                $ Declaration.ValueSignature () badName
                $ SharedType.TypeApplication
                    (SharedType.TypeConstructor phantomName)
                    (SharedType.TypeConstructor higherName)
            ]
      KindInference.inferDeclarationKinds
          (sealedEnvironment declarations) @?= Left
        (KindInference.DeclarationKindError badName
          $ KindInference.KindMismatch proper $ arrow proper proper)
  , testCase "generalize unconstrained classes while propagating known superclasses" $ do
      let intName = right $ mkIdentifier "Int"
          typeableName = right $ mkIdentifier "Typeable"
          dataName = right $ mkIdentifier "Data"
          functorName = right $ mkIdentifier "Functor"
          derivedName = right $ mkIdentifier "Derived"
          observeName = right $ mkIdentifier "observe"
          castName = right $ mkIdentifier "castLike"
          mapName = right $ mkIdentifier "mapLike"
          useName = right $ mkIdentifier "useTypeableTwice"
          parameter parameterName =
            Declaration.TypeParameter parameterName Nothing
          variable = SharedType.TypeVariable
          application = SharedType.TypeApplication
          function = SharedType.FunctionType
          constraint className argument = Constraint className [argument]
          declarations :: [Declaration.Declaration String Void ()]
          declarations =
            [ Declaration.AbstractTypeDeclaration () intName proper
            -- Deliberately precede Typeable: inference must not depend on
            -- lexical declaration order when a superclass is poly-kinded.
            , Declaration.ClassDeclaration () dataName [parameter "a"]
                [constraint typeableName $ variable "a"]
                [ Declaration.ValueSignature () observeName
                    $ function (variable "a")
                        (SharedType.TypeConstructor intName)
                , Declaration.ValueSignature () castName
                    $ SharedType.ForallType []
                        [constraint typeableName $ variable "f"]
                    $ function
                        (application (variable "f")
                          $ SharedType.TypeConstructor intName)
                        (variable "a")
                ]
            , Declaration.ClassDeclaration () typeableName
                [parameter "a"] [] []
            , Declaration.ClassDeclaration () functorName
                [parameter "f"] []
                [ Declaration.ValueSignature () mapName
                    $ function (function (variable "a") $ variable "b")
                    $ function
                        (application (variable "f") $ variable "a")
                        (application (variable "f") $ variable "b")
                ]
            , Declaration.ClassDeclaration () derivedName
                [parameter "f"]
                [constraint functorName $ variable "f"] []
            -- Operational signatures run only after class kinds stabilize.
            -- The generalized parameter must be instantiated independently
            -- at proper and constructor kinds in the same signature.
            , Declaration.ValueDeclaration
                $ Declaration.ValueSignature () useName
                $ SharedType.ForallType ["a", "f"]
                    [ constraint typeableName $ variable "a"
                    , constraint typeableName $ variable "f"
                    ]
                $ function
                    (application (variable "f")
                      $ SharedType.TypeConstructor intName)
                    (variable "a")
            ]
      KindInference.inferDeclarationKinds
          (sealedEnvironment declarations) @?= Right
        (KindInference.KindAssumptions
          (Map.singleton intName proper)
          (Map.fromList
            [ (dataName, [Just proper])
            , (derivedName, [Just $ arrow proper proper])
            , (functorName, [Just $ arrow proper proper])
            , (typeableName, [Nothing])
            ]))
  , testCase "propagate fixed kinds through a reversed three-link chain" $ do
      let firstName = right $ mkIdentifier "First"
          secondName = right $ mkIdentifier "Second"
          thirdName = right $ mkIdentifier "Third"
          seedName = right $ mkIdentifier "Seed"
          mapName = right $ mkIdentifier "mapSeed"
          parameter = Declaration.TypeParameter "f" Nothing
          variable = SharedType.TypeVariable
          application = SharedType.TypeApplication
          function = SharedType.FunctionType
          superclass owner = Constraint owner [variable "f"]
          classDeclaration owner superclasses methods =
            Declaration.ClassDeclaration () owner [parameter]
              superclasses methods
          seedMethod = Declaration.ValueSignature () mapName
            $ function (function (variable "a") $ variable "b")
            $ function
                (application (variable "f") $ variable "a")
                (application (variable "f") $ variable "b")
          declarations :: [Declaration.Declaration String Void ()]
          declarations =
            -- Every dependency follows its declaration. A single snapshot
            -- pass cannot move Seed's kind all the way back to First.
            [ classDeclaration firstName [superclass secondName] []
            , classDeclaration secondName [superclass thirdName] []
            , classDeclaration thirdName [superclass seedName] []
            , classDeclaration seedName [] [seedMethod]
            ]
          constructorKind = Just $ arrow proper proper
      KindInference.inferDeclarationKinds
          (sealedEnvironment declarations) @?= Right
        (KindInference.KindAssumptions Map.empty $ Map.fromList
          [ (firstName, [constructorKind])
          , (secondName, [constructorKind])
          , (thirdName, [constructorKind])
          , (seedName, [constructorKind])
          ])
  , testCase "propagate a seed through a mutual superclass cycle" $ do
      let aName = right $ mkIdentifier "A"
          bName = right $ mkIdentifier "B"
          mapName = right $ mkIdentifier "mapA"
          parameter = Declaration.TypeParameter "f" Nothing
          variable = SharedType.TypeVariable
          application = SharedType.TypeApplication
          function = SharedType.FunctionType
          superclass owner = Constraint owner [variable "f"]
          seedMethod = Declaration.ValueSignature () mapName
            $ function (function (variable "a") $ variable "b")
            $ function
                (application (variable "f") $ variable "a")
                (application (variable "f") $ variable "b")
          declarations :: [Declaration.Declaration String Void ()]
          declarations =
            [ Declaration.ClassDeclaration () aName [parameter]
                [superclass bName] [seedMethod]
            , Declaration.ClassDeclaration () bName [parameter]
                [superclass aName] []
            ]
          constructorKind = Just $ arrow proper proper
      KindInference.inferDeclarationKinds
          (sealedEnvironment declarations) @?= Right
        (KindInference.KindAssumptions Map.empty $ Map.fromList
          [ (aName, [constructorKind])
          , (bName, [constructorKind])
          ])
  , testCase "leave an unseeded mutual superclass cycle generalized" $ do
      let aName = right $ mkIdentifier "A"
          bName = right $ mkIdentifier "B"
          parameter = Declaration.TypeParameter "a" Nothing
          variable = SharedType.TypeVariable "a"
          declarations :: [Declaration.Declaration String Void ()]
          declarations =
            [ Declaration.ClassDeclaration () aName [parameter]
                [Constraint bName [variable]] []
            , Declaration.ClassDeclaration () bName [parameter]
                [Constraint aName [variable]] []
            ]
      KindInference.inferDeclarationKinds
          (sealedEnvironment declarations) @?= Right
        (KindInference.KindAssumptions Map.empty $ Map.fromList
          [(aName, [Nothing]), (bName, [Nothing])])
  , testCase "select generalized or Haskell 98 defaulted class kinds" $ do
      let markerName = right $ mkIdentifier "Marker"
          parameter = Declaration.TypeParameter "a" Nothing
          declarations :: [Declaration.Declaration String Void ()]
          declarations =
            [Declaration.ClassDeclaration () markerName [parameter] [] []]
      KindInference.inferDeclarationKindsWithClassPolicy
          KindInference.ClosedKindInventory
          KindInference.GeneralizeClassKinds
          (sealedEnvironment declarations) @?= Right
        (KindInference.KindAssumptions Map.empty
          $ Map.singleton markerName [Nothing])
      KindInference.inferDeclarationKindsWithClassPolicy
          KindInference.ClosedKindInventory
          KindInference.DefaultClassKinds
          (sealedEnvironment declarations) @?= Right
        (KindInference.KindAssumptions Map.empty
          $ Map.singleton markerName [Just proper])
  , testCase "default class kinds before checking instances" $ do
      let higherName = right $ mkIdentifier "Higher"
          markerName = right $ mkIdentifier "Marker"
          parameter = Declaration.TypeParameter "a" Nothing
          higherKind = arrow proper proper
          declarations :: [Declaration.Declaration String Void ()]
          declarations =
            [ Declaration.AbstractTypeDeclaration () higherName higherKind
            , Declaration.ClassDeclaration () markerName [parameter] [] []
            , Declaration.InstanceDeclaration () [] []
                $ Constraint markerName
                    [SharedType.TypeConstructor higherName]
            ]
      KindInference.inferDeclarationKindsWithClassPolicy
          KindInference.ClosedKindInventory
          KindInference.DefaultClassKinds
          (sealedEnvironment declarations) @?= Left
        (KindInference.DeclarationKindError markerName
          $ KindInference.KindMismatch higherKind proper)
  , testCase "recheck defining superclasses after class kind defaulting" $ do
      let higherName = right $ mkIdentifier "Higher"
          markerName = right $ mkIdentifier "Marker"
          derivedName = right $ mkIdentifier "Derived"
          parameter = Declaration.TypeParameter "a" Nothing
          higherKind = arrow proper proper
          declarations :: [Declaration.Declaration String Void ()]
          declarations =
            [ Declaration.AbstractTypeDeclaration () higherName higherKind
            , Declaration.ClassDeclaration () markerName [parameter] [] []
            -- Marker is generalized during the defining fixpoint, so this
            -- edge becomes invalid only after Haskell 98 defaulting.
            , Declaration.ClassDeclaration () derivedName []
                [Constraint markerName
                  [SharedType.TypeConstructor higherName]] []
            ]
      KindInference.inferDeclarationKindsWithClassPolicy
          KindInference.ClosedKindInventory
          KindInference.DefaultClassKinds
          (sealedEnvironment declarations) @?= Left
        (KindInference.DeclarationKindError derivedName
          $ KindInference.KindMismatch higherKind proper)
  , testCase "freeze residual variables below fixed class kinds" $ do
      let unitName = right $ mkIdentifier "Unit"
          higherName = right $ mkIdentifier "Higher"
          appliedName = right $ mkIdentifier "Applied"
          applyName = right $ mkIdentifier "apply"
          parameter = Declaration.TypeParameter "f" Nothing
          variable = SharedType.TypeVariable
          higherKind = arrow (arrow proper proper) proper
          methodType = SharedType.FunctionType
            (SharedType.TypeApplication (variable "f") $ variable "a")
            (SharedType.TypeConstructor unitName)
          declarations :: [Declaration.Declaration String Void ()]
          declarations =
            [ Declaration.AbstractTypeDeclaration () unitName proper
            , Declaration.AbstractTypeDeclaration () higherName higherKind
            -- The method fixes only the outer shape f :: k -> Type. Since the
            -- shared IR has no partial kind schemes, k must freeze to Type
            -- before an instance can specialize it.
            , Declaration.ClassDeclaration () appliedName [parameter] []
                [Declaration.ValueSignature () applyName methodType]
            , Declaration.InstanceDeclaration () [] []
                $ Constraint appliedName
                    [SharedType.TypeConstructor higherName]
            ]
      KindInference.inferDeclarationKindsWithClassPolicy
          KindInference.ClosedKindInventory
          KindInference.GeneralizeClassKinds
          (sealedEnvironment declarations) @?= Left
        (KindInference.DeclarationKindError appliedName
          $ KindInference.KindMismatch (arrow proper proper) proper)
  , testCase "default an unseeded mutual superclass cycle" $ do
      let aName = right $ mkIdentifier "A"
          bName = right $ mkIdentifier "B"
          parameter = Declaration.TypeParameter "a" Nothing
          variable = SharedType.TypeVariable "a"
          declarations :: [Declaration.Declaration String Void ()]
          declarations =
            [ Declaration.ClassDeclaration () aName [parameter]
                [Constraint bName [variable]] []
            , Declaration.ClassDeclaration () bName [parameter]
                [Constraint aName [variable]] []
            ]
      KindInference.inferDeclarationKindsWithClassPolicy
          KindInference.ClosedKindInventory
          KindInference.DefaultClassKinds
          (sealedEnvironment declarations) @?= Right
        (KindInference.KindAssumptions Map.empty $ Map.fromList
          [ (aName, [Just proper])
          , (bName, [Just proper])
          ])
  , testCase "report fixed superclass conflicts at the first declaration" $ do
      let clashName = right $ mkIdentifier "Clash"
          properName = right $ mkIdentifier "Proper"
          higherName = right $ mkIdentifier "Higher"
          properMethodName = right $ mkIdentifier "properMethod"
          higherMethodName = right $ mkIdentifier "higherMethod"
          parameter = Declaration.TypeParameter "f" Nothing
          variable = SharedType.TypeVariable
          application = SharedType.TypeApplication
          function = SharedType.FunctionType
          superclass owner = Constraint owner [variable "f"]
          properMethod = Declaration.ValueSignature () properMethodName
            $ function (variable "f") (variable "f")
          higherMethod = Declaration.ValueSignature () higherMethodName
            $ function
                (application (variable "f") $ variable "a")
                (variable "a")
          declarations :: [Declaration.Declaration String Void ()]
          declarations =
            [ Declaration.ClassDeclaration () clashName [parameter]
                [superclass properName, superclass higherName] []
            , Declaration.ClassDeclaration () properName [parameter]
                [] [properMethod]
            , Declaration.ClassDeclaration () higherName [parameter]
                [] [higherMethod]
            ]
      KindInference.inferDeclarationKinds
          (sealedEnvironment declarations) @?= Left
        (KindInference.DeclarationKindError clashName
          $ KindInference.KindMismatch proper $ arrow proper proper)
  , testCase "reject recursive synonyms in a whole inventory" $ do
      let aName = right $ mkIdentifier "A"
          bName = right $ mkIdentifier "B"
          synonym name reference = Declaration.TypeSynonymDeclaration
            () name [] $ SharedType.TypeConstructor reference
          declarations :: [Declaration.Declaration String Void ()]
          declarations = [synonym aName bName, synonym bName aName]
      KindInference.inferDeclarationKinds
          (sealedEnvironment declarations) @?=
        Left (KindInference.RecursiveTypeDeclarations [aName, bName])
  , testCase "infer external names in an open inventory" $ do
      let externalType = right $ mkIdentifier "External"
          externalClass = right $ mkIdentifier "ExternalClass"
          valueName = right $ mkIdentifier "value"
          signature = SharedType.ForallType ["a"]
            [Constraint externalClass [SharedType.TypeVariable "a"]]
            (SharedType.TypeConstructor externalType)
          declarations :: [Declaration.Declaration String Void ()]
          declarations = [Declaration.ValueDeclaration
            $ Declaration.ValueSignature () valueName signature]
      KindInference.inferDeclarationKindsWith
          KindInference.OpenKindInventory
          (sealedEnvironment declarations) @?= Right
        (KindInference.KindAssumptions
          (Map.singleton externalType proper)
          (Map.singleton externalClass [Nothing]))
  , testCase "reject inconsistent external class arities" $ do
      let externalClass = right $ mkIdentifier "ExternalClass"
          valueName spelling = right $ mkIdentifier spelling
          constrained arguments = SharedType.ForallType ["a", "b"]
            [Constraint externalClass arguments]
            (SharedType.TypeVariable "a")
          declaration name arguments = Declaration.ValueDeclaration
            $ Declaration.ValueSignature () (valueName name)
            $ constrained arguments
          variable name = SharedType.TypeVariable name
          declarations :: [Declaration.Declaration String Void ()]
          declarations =
            [ declaration "first" [variable "a"]
            , declaration "second" [variable "a", variable "b"]
            ]
      KindInference.inferDeclarationKindsWith
          KindInference.OpenKindInventory
          (sealedEnvironment declarations) @?=
        Left (KindInference.ClassArityMismatch externalClass 1 2)
  ]
 where
  proper :: KindInference.GroundKind
  proper = Kind.ProperTypeKind
  arrow = Kind.FunctionKind

environmentTests :: TestTree
environmentTests = testGroup "environments"
  [ testCase "seal structural indexes with their kind assumptions" $ do
      let typeName = right $ mkIdentifier "T"
          constructorName = right $ mkIdentifier "MkT"
          parameter = Declaration.TypeParameter "a" Nothing
          declarations :: [Declaration.Declaration String Void ()]
          declarations =
            [ Declaration.DataTypeDeclaration () typeName [parameter]
                [Declaration.DataConstructor () constructorName []]
            ]
          inventory = right $ Inventory.mkInventory
            KindInference.ClosedKindInventory declarations
      Map.keys (Environment.typeDeclarationMap
        $ Inventory.inventoryEnvironment inventory) @?= [typeName]
      KindInference.typeConstructorKinds
        (Inventory.inventoryKindAssumptions inventory) @?=
          Map.singleton typeName
            (Kind.FunctionKind Kind.ProperTypeKind Kind.ProperTypeKind)
  , testCase "thread class kind finalization through inventories" $ do
      let markerName = right $ mkIdentifier "Marker"
          declaration :: Declaration.Declaration String Void ()
          declaration = Declaration.ClassDeclaration () markerName
            [Declaration.TypeParameter "a" Nothing] [] []
          generalized = right $ Inventory.mkInventory
            KindInference.ClosedKindInventory [declaration]
          defaulted = right $ Inventory.mkInventoryWithClassPolicy
            KindInference.ClosedKindInventory
            KindInference.DefaultClassKinds [declaration]
          classKinds = KindInference.classParameterKinds
            . Inventory.inventoryKindAssumptions
      classKinds generalized @?= Map.singleton markerName [Nothing]
      classKinds defaulted @?=
        Map.singleton markerName [Just Kind.ProperTypeKind]
  , testCase "construct equivalent inventories from lists and environments" $ do
      let intName = right $ mkIdentifier "Int"
          markerName = right $ mkIdentifier "Marker"
          declarations :: [Declaration.Declaration String Void ()]
          declarations =
            [ Declaration.AbstractTypeDeclaration () intName
                Kind.ProperTypeKind
            , Declaration.ClassDeclaration () markerName
                [Declaration.TypeParameter "a" Nothing] [] []
            ]
          environment = right $ Environment.mkEnvironment declarations
      Inventory.mkInventory KindInference.ClosedKindInventory declarations @?=
        Inventory.mkInventoryFromEnvironmentWithClassPolicy
          KindInference.ClosedKindInventory
          KindInference.GeneralizeClassKinds environment
      Inventory.mkInventoryWithClassPolicy
          KindInference.ClosedKindInventory
          KindInference.DefaultClassKinds declarations @?=
        Inventory.mkInventoryFromEnvironmentWithClassPolicy
          KindInference.ClosedKindInventory
          KindInference.DefaultClassKinds environment
  , testCase "erase inventory annotations without rebuilding its indexes" $ do
      let typeName = right $ mkIdentifier "T"
          constructorName = right $ mkIdentifier "MkT"
          declarations :: [Declaration.Declaration String Void Int]
          declarations =
            [ Declaration.DataTypeDeclaration 1 typeName []
                [Declaration.DataConstructor 2 constructorName []]
            ]
          inventory = right $ Inventory.mkInventory
            KindInference.ClosedKindInventory declarations
          erased = fmap (const ()) inventory
          expected = map (fmap $ const ()) declarations
      Environment.environmentDeclarations
          (Inventory.inventoryEnvironment erased) @?= expected
      Map.keys (Environment.typeDeclarationMap
          $ Inventory.inventoryEnvironment erased) @?= [typeName]
      Map.keys (Environment.dataConstructorMap
          $ Inventory.inventoryEnvironment erased) @?= [constructorName]
      Inventory.inventoryKindAssumptions erased @?=
        Inventory.inventoryKindAssumptions inventory
  , testCase "inventories retain the first unsolved declaration kind" $ do
      let typeName = right $ mkIdentifier "T"
          declarations :: [Declaration.Declaration String String ()]
          declarations =
            [Declaration.AbstractTypeDeclaration () typeName
              $ Kind.FunctionKind (Kind.KindVariable "k")
                  Kind.ProperTypeKind]
          environment = right $ Environment.mkEnvironment declarations
          expected = Left $ Inventory.UngroundedInventoryKind "k"
      Inventory.mkInventory KindInference.ClosedKindInventory declarations @?=
        expected
      Environment.groundEnvironmentKinds environment @?= Left "k"
  , testCase "report structural errors before unsolved declaration kinds" $ do
      let typeName = right $ mkIdentifier "T"
          declarations :: [Declaration.Declaration String String ()]
          declarations =
            [ Declaration.AbstractTypeDeclaration () typeName
                $ Kind.KindVariable "k"
            , Declaration.AbstractTypeDeclaration () typeName
                Kind.ProperTypeKind
            ]
      Inventory.mkInventory KindInference.ClosedKindInventory declarations @?=
        Left (Inventory.InvalidInventoryEnvironment
          $ Environment.DuplicateTypeDeclaration typeName)
  , testCase "index declarations across shared namespaces" $ do
      let typeName = right $ mkIdentifier "T"
          constructorName = right $ mkIdentifier "MkT"
          valueName = right $ mkIdentifier "makeT"
          className = right $ mkIdentifier "C"
          methodName = right $ mkIdentifier "method"
          declarations :: [Declaration.Declaration String Int ()]
          declarations =
            [ Declaration.DataTypeDeclaration () typeName []
                [Declaration.DataConstructor () constructorName []]
            , Declaration.ValueDeclaration
                $ Declaration.ValueSignature () valueName
                $ SharedType.TypeConstructor typeName
            , Declaration.ClassDeclaration () className [] []
                [Declaration.ValueSignature () methodName
                  $ SharedType.TypeConstructor typeName]
            ]
      let environment = right $ Environment.mkEnvironment declarations
      Environment.environmentDeclarations environment @?= declarations
      Map.keys (Environment.typeDeclarationMap environment) @?=
        [className, typeName]
      Map.keys (Environment.valueSignatureMap environment) @?=
        [valueName, methodName]
      Map.keys (Environment.dataConstructorMap environment) @?=
        [constructorName]
  , testCase "reject duplicate type, value, and instance declarations" $ do
      let typeName = right $ mkIdentifier "T"
          valueName = right $ mkIdentifier "value"
          className = right $ mkIdentifier "C"
          typeDeclaration :: Declaration.Declaration String Int ()
          typeDeclaration = Declaration.AbstractTypeDeclaration
            () typeName Kind.ProperTypeKind
          valueDeclaration :: Declaration.Declaration String Int ()
          valueDeclaration = Declaration.ValueDeclaration
            $ Declaration.ValueSignature () valueName
            $ SharedType.TypeConstructor typeName
          instanceDeclaration :: Declaration.Declaration String Int ()
          instanceDeclaration = Declaration.InstanceDeclaration
            () [] [] (Constraint className [])
      Environment.mkEnvironment [typeDeclaration, typeDeclaration] @?=
        Left (Environment.DuplicateTypeDeclaration typeName)
      Environment.mkEnvironment [valueDeclaration, valueDeclaration] @?=
        Left (Environment.DuplicateValueDeclaration valueName)
      Environment.mkEnvironment [instanceDeclaration, instanceDeclaration] @?=
        Left (Environment.DuplicateInstanceDeclaration
          $ Constraint className [])
  , testCase "reject alpha-equivalent instance heads with source diagnostics" $ do
      let className = right $ mkIdentifier "C"
          firstHead = Constraint className
            [SharedType.TypeVariable "a"]
          renamedHead = Constraint className
            [SharedType.TypeVariable "renamed"]
          first = environmentInstance className ["a"]
            $ constraintArguments firstHead
          renamed = environmentInstance className ["renamed"]
            $ constraintArguments renamedHead
          environment = right $ Environment.mkEnvironment [first]
      Map.keys (Environment.instanceDeclarationMap environment) @?= [firstHead]
      Environment.mkEnvironment [first, renamed] @?=
        Left (Environment.DuplicateInstanceDeclaration renamedHead)
      _ <- evaluate $ force environment
      pure ()
  , testCase "ignore instance binder spelling and declaration order" $ do
      let className = right $ mkIdentifier "PairClass"
          variable = SharedType.TypeVariable
          first = environmentInstance className ["a", "b"]
            [variable "a", variable "b"]
          reorderedHead = Constraint className
            [variable "x", variable "y"]
          reordered = environmentInstance className ["y", "x"]
            $ constraintArguments reorderedHead
      Environment.mkEnvironment [first, reordered] @?=
        Left (Environment.DuplicateInstanceDeclaration reorderedHead)
  , testCase "canonicalize nested forall binders by scope and occurrence" $ do
      let className = right $ mkIdentifier "Nested"
          innerClassName = right $ mkIdentifier "Inner"
          variable = SharedType.TypeVariable
          firstType = SharedType.ForallType ["innerA", "innerB"]
            [Constraint innerClassName [variable "innerB"]]
            $ SharedType.TupleType Boxed
                [variable "innerA", variable "outer", variable "innerB"]
          renamedType = SharedType.ForallType ["right", "left"]
            [Constraint innerClassName [variable "right"]]
            $ SharedType.TupleType Boxed
                [variable "left", variable "renamedOuter", variable "right"]
          first = environmentInstance className ["outer"] [firstType]
          renamedHead = Constraint className [renamedType]
          renamed = environmentInstance className ["renamedOuter"] [renamedType]
      Environment.mkEnvironment [first, renamed] @?=
        Left (Environment.DuplicateInstanceDeclaration renamedHead)
  , testCase "respect nested forall shadowing" $ do
      let className = right $ mkIdentifier "Scoped"
          variable = SharedType.TypeVariable
          shadowingType outer inner = SharedType.FunctionType
            (variable outer)
            (SharedType.ForallType [inner] [] $ variable inner)
          alphaRenamed = environmentInstance className ["outer"]
            [shadowingType "outer" "inner"]
          sourceShadowing = environmentInstance className ["a"]
            [shadowingType "a" "a"]
          capturesOuter = environmentInstance className ["x"]
            [ SharedType.FunctionType (variable "x")
                (SharedType.ForallType ["inner"] [] $ variable "x")
            ]
      Environment.mkEnvironment [sourceShadowing, alphaRenamed] @?=
        Left (Environment.DuplicateInstanceDeclaration
          $ Constraint className [shadowingType "outer" "inner"])
      Map.size (Environment.instanceDeclarationMap $ right
          $ Environment.mkEnvironment [sourceShadowing, capturesOuter]) @?= 2
  , testCase "preserve sharing and qualified identity in instance heads" $ do
      let namespaceA = right $ mkModuleName "A"
          namespaceB = right $ mkModuleName "B"
          classA = right $ mkQualifiedIdentifier namespaceA "C"
          classB = right $ mkQualifiedIdentifier namespaceB "C"
          typeA = right $ mkQualifiedIdentifier namespaceA "T"
          typeB = right $ mkQualifiedIdentifier namespaceB "T"
          variable = SharedType.TypeVariable
          repeated = environmentInstance classA ["a"]
            [variable "a", variable "a"]
          distinct = environmentInstance classA ["x", "y"]
            [variable "x", variable "y"]
          qualified = environmentInstance classB ["z"] [variable "z"]
          constructedA = environmentInstance classA []
            [SharedType.TypeConstructor typeA]
          constructedB = environmentInstance classA []
            [SharedType.TypeConstructor typeB]
          environment = right
            $ Environment.mkEnvironment
                [repeated, distinct, qualified, constructedA, constructedB]
      Map.size (Environment.instanceDeclarationMap environment) @?= 5
  , testCase "report invalid declarations before alpha duplicates" $ do
      let className = right $ mkIdentifier "C"
          variable = SharedType.TypeVariable "a"
          first = environmentInstance className ["a"] [variable]
          invalid = environmentInstance className ["a", "a"] [variable]
      Environment.mkEnvironment [first, invalid] @?= Left
        (Environment.InvalidEnvironmentDeclaration 1
          $ Declaration.DuplicateTypeParameter "a")
  , testCase "qualified type names remain nominally distinct" $ do
      let namespaceA = right $ mkModuleName "A"
          namespaceB = right $ mkModuleName "B"
          typeA = right $ mkQualifiedIdentifier namespaceA "T"
          typeB = right $ mkQualifiedIdentifier namespaceB "T"
          declarations :: [Declaration.Declaration String Int ()]
          declarations =
            [ Declaration.AbstractTypeDeclaration
                () typeA Kind.ProperTypeKind
            , Declaration.AbstractTypeDeclaration
                () typeB Kind.ProperTypeKind
            ]
          environment = right $ Environment.mkEnvironment declarations
      Map.size (Environment.typeDeclarationMap environment) @?= 2
  , testCase "inventories can represent trusted intrinsic types" $ do
      let unitName = right $ tupleName Boxed 0
          unitDeclaration :: Declaration.Declaration String Int ()
          unitDeclaration = Declaration.DataTypeDeclaration () unitName []
            [Declaration.DataConstructor () unitName []]
          environment = right $ Environment.mkEnvironment [unitDeclaration]
      Map.member unitName (Environment.typeDeclarationMap environment) @?= True
      Map.member unitName (Environment.dataConstructorMap environment) @?= True
  , testCase "retain the failing declaration index" $ do
      let invalidName = right $ mkIdentifier "value"
          invalid :: Declaration.Declaration String Int ()
          invalid = Declaration.TypeSynonymDeclaration
            () invalidName [] $ SharedType.TypeVariable "a"
      Environment.mkEnvironment [invalid] @?= Left
        (Environment.InvalidEnvironmentDeclaration 0
          $ Declaration.InvalidDeclaredTypeName invalidName)
  ]
 where
  environmentInstance
    :: Name
    -> [String]
    -> [SharedType.Type String]
    -> Declaration.Declaration String Int ()
  environmentInstance className variables arguments =
    Declaration.InstanceDeclaration () variables []
      $ Constraint className arguments

declarationTests :: TestTree
declarationTests = testGroup "declarations"
  [ testCase "validate kinded data and class declarations" $ do
      let maybeName = right $ mkIdentifier "Maybe"
          justName = right $ mkIdentifier "Just"
          nothingName = right $ mkIdentifier "Nothing"
          eqName = right $ mkIdentifier "Eq"
          equalsName = right $ mkOperator "=="
          parameter = Declaration.TypeParameter "a"
            (Just Kind.ProperTypeKind)
          variable = SharedType.TypeVariable "a"
          maybeDeclaration = Declaration.DataTypeDeclaration () maybeName
            [parameter]
            [ Declaration.DataConstructor () nothingName []
            , Declaration.DataConstructor () justName [variable]
            ]
          eqDeclaration = Declaration.ClassDeclaration () eqName [parameter] []
            [Declaration.ValueSignature () equalsName
              $ SharedType.FunctionType variable
              $ SharedType.FunctionType variable
              $ SharedType.TypeConstructor (right $ mkIdentifier "Bool")]
      Declaration.validateDeclaration maybeDeclaration @?= Right ()
      Declaration.validateDeclaration eqDeclaration @?= Right ()
  , testCase "reject namespace and duplicate-member mistakes" $ do
      let typeName = right $ mkIdentifier "T"
          valueName = right $ mkIdentifier "value"
          variable = SharedType.TypeVariable "a"
          duplicateConstructors ::
            Declaration.Declaration String Int ()
          duplicateConstructors = Declaration.DataTypeDeclaration () typeName []
            [ Declaration.DataConstructor () typeName []
            , Declaration.DataConstructor () typeName []
            ]
          invalidConstrainedValue ::
            Declaration.Declaration String Int ()
          invalidConstrainedValue = Declaration.ValueDeclaration $
            Declaration.ValueSignature () valueName $
              SharedType.ForallType [] [Constraint valueName []] $
                SharedType.TypeConstructor typeName
          invalidClass :: Declaration.Declaration String Int ()
          invalidClass = Declaration.ClassDeclaration
            () valueName [] [] []
      Declaration.validateDeclaration
          (Declaration.TypeSynonymDeclaration () valueName [] variable)
        @?= Left (Declaration.InvalidDeclaredTypeName valueName)
      Declaration.validateDeclaration duplicateConstructors
        @?= Left (Declaration.DuplicateConstructorName typeName)
      Declaration.validateDeclaration invalidClass
        @?= Left (Declaration.InvalidClassName valueName)
      Declaration.validateDeclaration invalidConstrainedValue @?= Left
        (Declaration.InvalidDeclarationType $ SharedType.InvalidTypeConstraint
          $ InvalidConstraintClass valueName)
      Declaration.validateDeclaration
          (Declaration.TypeSynonymDeclaration () typeName
            [ Declaration.TypeParameter "a" Nothing
            , Declaration.TypeParameter "a" Nothing
            ] variable)
        @?= Left (Declaration.DuplicateTypeParameter "a")
  , testCase "validate instance heads without claiming resolution" $ do
      let eqName = right $ mkIdentifier "Eq"
          variable = SharedType.TypeVariable "a"
          constraint = Constraint eqName [variable]
          declaration :: Declaration.Declaration String Int ()
          declaration = Declaration.InstanceDeclaration () ["a"]
            [constraint] constraint
      Declaration.validateDeclaration declaration @?= Right ()
      _ <- evaluate $ force declaration
      pure ()
  , testCase "map and traverse backend annotations at every declaration site" $ do
      let typeName = right $ mkIdentifier "T"
          firstName = right $ mkIdentifier "First"
          secondName = right $ mkIdentifier "Second"
          declaration :: Declaration.Declaration String Int Int
          declaration = Declaration.DataTypeDeclaration 1 typeName []
            [ Declaration.DataConstructor 2 firstName []
            , Declaration.DataConstructor 3 secondName []
            ]
          expected = Declaration.DataTypeDeclaration 11 typeName []
            [ Declaration.DataConstructor 12 firstName []
            , Declaration.DataConstructor 13 secondName []
            ]
      fmap (+ 10) declaration @?= expected
      toList declaration @?= [1, 2, 3]
      traverse (Just . (+ 10)) declaration @?= Just expected
  , testCase "kind variables traverse and report free identities" $ do
      let kind = Kind.FunctionKind (Kind.KindVariable (1 :: Int))
            Kind.ProperTypeKind
      Kind.freeKindVariables kind @?= Set.singleton 1
      fmap (+ 1) kind @?=
        Kind.FunctionKind (Kind.KindVariable 2) Kind.ProperTypeKind
  , testCase "declaration binders cover every non-implicit variable" $ do
      let typeName = right $ mkIdentifier "T"
          constructorName = right $ mkIdentifier "MkT"
          className = right $ mkIdentifier "C"
          parameter = Declaration.TypeParameter "a" Nothing
          unbound = SharedType.TypeVariable "b"
          superclass = Constraint className [unbound]
          synonym :: Declaration.Declaration String Int ()
          synonym = Declaration.TypeSynonymDeclaration () typeName
            [parameter] unbound
          datatype = Declaration.DataTypeDeclaration () typeName [parameter]
            [Declaration.DataConstructor () constructorName [unbound]]
          classDeclaration = Declaration.ClassDeclaration () className
            [parameter] [superclass] []
          instanceDeclaration = Declaration.InstanceDeclaration () ["a"]
            [] superclass
      Declaration.validateDeclaration synonym @?= Left
        (Declaration.UndeclaredSynonymVariables typeName ["b"])
      Declaration.validateDeclaration datatype @?= Left
        (Declaration.UndeclaredDataVariables typeName ["b"])
      Declaration.validateDeclaration classDeclaration @?= Left
        (Declaration.UndeclaredSuperclassVariables className ["b"])
      Declaration.validateDeclaration instanceDeclaration @?= Left
        (Declaration.UndeclaredInstanceVariables className ["b"])
  , testCase "classify recursive datatype components structurally" $ do
      let directName = right $ mkIdentifier "Direct"
          leftName = right $ mkIdentifier "LeftRec"
          rightName = right $ mkIdentifier "RightRec"
          acyclicName = right $ mkIdentifier "Acyclic"
          duplicateName = right $ mkIdentifier "Duplicate"
          externalName = right $ mkIdentifier "External"
          constructorName = right $ mkIdentifier "Make"
          datatype name references = Declaration.DataTypeDeclaration
            () name []
            [ Declaration.DataConstructor () constructorName
                (map SharedType.TypeConstructor references)
            ]
          declarations :: [Declaration.Declaration String Int ()]
          declarations =
            [ datatype directName [directName]
            , datatype leftName [rightName]
            , datatype rightName [leftName]
            , datatype acyclicName [externalName]
            -- The structural classifier stays deterministic even before the
            -- Environment boundary rejects this duplicate head.
            , datatype duplicateName []
            , datatype duplicateName [duplicateName]
            ]
      Declaration.recursiveDataTypeNames declarations @?=
        Set.fromList [directName, leftName, rightName, duplicateName]
  ]

typeTests :: TestTree
typeTests = testGroup "source types"
  [ testCase "render tagged variables without conflating their identities" $ do
      let className = right $ mkIdentifier "C"
          flexible = Left (0 :: Int)
          rigid = Right (0 :: Int)
          variableName variable = case variable of
            Left _ -> "a"
            Right _ -> "skolem"
          typeExpression = SharedType.FunctionType
            (SharedType.TypeVariable flexible)
            (SharedType.TypeVariable rigid)
          constraint = Constraint className
            [SharedType.TypeVariable flexible,
             SharedType.TypeVariable rigid]
      TypeRender.renderType variableName typeExpression @?=
        "a -> skolem"
      TypeRender.renderConstraint variableName constraint @?=
        "C a skolem"
      TypeRender.showsType variableName 2 typeExpression "" @?=
        "(a -> skolem)"
      TypeRender.showsConstraint variableName 1 constraint "" @?=
        "(C a skolem)"
      TypeRender.showsType variableName 2
          (SharedType.ForallType [] [] typeExpression) "" @?=
        "(a -> skolem)"
      TypeRender.renderType variableName
          (SharedType.TupleType Unboxed []) @?= "(# #)"
      TypeRender.renderType variableName
          (SharedType.TupleType Unboxed
            [SharedType.TypeVariable flexible]) @?= "(# a #)"
  , testCase "canonicalize saturated function and tuple constructors" $ do
      let a = SharedType.TypeVariable "a"
          b = SharedType.TypeVariable "b"
          arrow = SharedType.TypeApplication
            (SharedType.TypeApplication
              (SharedType.TypeConstructor functionName) a) b
          pairName = right $ tupleName Boxed 2
          pair = SharedType.TypeApplication
            (SharedType.TypeApplication
              (SharedType.TypeConstructor pairName) a) b
      SharedType.canonicalizeType arrow @?=
        SharedType.FunctionType a b
      SharedType.canonicalizeType pair @?=
        SharedType.TupleType Boxed [a, b]
      SharedType.canonicalizeType
          (SharedType.TypeConstructor (right $ tupleName Boxed 0)
            :: SharedType.Type String) @?=
        SharedType.TupleType Boxed []
      SharedType.canonicalizeType
          (SharedType.TypeConstructor (right $ tupleName Unboxed 0)
            :: SharedType.Type String) @?=
        SharedType.TupleType Unboxed []
  , testCase "decompose application spines in source order" $ do
      let headType = SharedType.TypeVariable "f"
          first = SharedType.TypeVariable "a"
          second = SharedType.TypeVariable "b"
          application = SharedType.TypeApplication
            (SharedType.TypeApplication headType first) second
      SharedType.applicationSpine application @?=
        (headType, [first, second])
      SharedType.applicationSpine first @?= (first, [])
  , testCase "forall binders protect bodies and constraints" $ do
      let className = right $ mkIdentifier "C"
          typeExpression = SharedType.ForallType ["a"]
            [Constraint className
              [SharedType.TypeVariable "a", SharedType.TypeVariable "b"]]
            (SharedType.FunctionType
              (SharedType.TypeVariable "a")
              (SharedType.TypeVariable "c"))
      SharedType.freeVariables typeExpression @?=
        Set.fromList ["b", "c"]
      SharedType.validateType typeExpression @?= Right ()
  , testCase "scoped renaming stops at shadowing foralls" $ do
      let className = right $ mkIdentifier "C"
          source = SharedType.FunctionType
            (SharedType.TypeVariable "a")
            $ SharedType.ForallType ["a"]
                [Constraint className
                  [ SharedType.TypeVariable "a"
                  , SharedType.TypeVariable "b"
                  ]]
                $ SharedType.FunctionType
                    (SharedType.TypeVariable "a")
                    (SharedType.TypeVariable "b")
          expected = SharedType.FunctionType
            (SharedType.TypeVariable "outer")
            $ SharedType.ForallType ["a"]
                [Constraint className
                  [ SharedType.TypeVariable "a"
                  , SharedType.TypeVariable "free"
                  ]]
                $ SharedType.FunctionType
                    (SharedType.TypeVariable "a")
                    (SharedType.TypeVariable "free")
      SharedType.renameScopedVariables
          (Map.fromList [("a", "outer"), ("b", "free")]) source
        @?= expected
  , testCase "reject malformed forall constraint class names" $ do
      let invalidName = right $ mkIdentifier "constraint"
          typeExpression = SharedType.ForallType ["a"]
            [Constraint invalidName [SharedType.TypeVariable "a"]]
            (SharedType.TypeVariable "a")
      SharedType.validateType typeExpression @?= Left
        (SharedType.InvalidTypeConstraint $ InvalidConstraintClass invalidName)
  , testCase "reject malformed tuples, constructors, and quantifiers" $ do
      let variableName = right $ mkIdentifier "value"
      SharedType.validateType
          (SharedType.TupleType Boxed [SharedType.TypeVariable (0 :: Int)])
        @?= Left (SharedType.InvalidTupleTypeArity Boxed 1)
      SharedType.validateType
          (SharedType.TupleType Unboxed [] :: SharedType.Type Int) @?= Right ()
      SharedType.validateType
          (SharedType.TupleType Unboxed
            [SharedType.TypeVariable (0 :: Int)]) @?= Right ()
      SharedType.validateType
          (SharedType.TupleType Unboxed
            $ replicate (maximumTupleArity + 1)
            $ SharedType.TypeVariable (0 :: Int)) @?=
        Left (SharedType.InvalidTupleTypeArity Unboxed
          $ maximumTupleArity + 1)
      SharedType.validateType
          (SharedType.TypeConstructor variableName :: SharedType.Type Int)
        @?= Left (SharedType.InvalidTypeConstructor variableName)
      SharedType.validateType
          (SharedType.ForallType [1 :: Int, 1] []
            $ SharedType.TypeVariable 1)
        @?= Left (SharedType.DuplicateForallVariable 1)
  , testCase "map, fold, traverse, and force every variable position" $ do
      let typeExpression = SharedType.ForallType [1 :: Int] []
            $ SharedType.FunctionType
              (SharedType.TypeVariable 1) (SharedType.TypeVariable 2)
      fmap (+ 10) typeExpression @?= SharedType.ForallType [11] []
        (SharedType.FunctionType
          (SharedType.TypeVariable 11) (SharedType.TypeVariable 12))
      sum typeExpression @?= 4
      traverse (Just . show) typeExpression @?=
        Just (SharedType.ForallType ["1"] []
          (SharedType.FunctionType
            (SharedType.TypeVariable "1") (SharedType.TypeVariable "2")))
      _ <- evaluate $ force typeExpression
      pure ()
  ]

synonymTests :: TestTree
synonymTests = testGroup "type synonyms"
  [ testCase "expand saturated and overapplied aliases" $ do
      let identityName = right $ mkIdentifier "Identity"
          maybeName = right $ mkIdentifier "Maybe"
          intName = right $ mkIdentifier "Int"
          declarations =
            [ Declaration.TypeSynonymDeclaration () identityName
                [Declaration.TypeParameter "a" Nothing]
                $ SharedType.TypeVariable "a"
            , Declaration.AbstractTypeDeclaration () maybeName
                $ Kind.FunctionKind synonymProper synonymProper
            , Declaration.AbstractTypeDeclaration () intName synonymProper
            ]
          aliases = preparedSynonyms declarations
          overapplied = SharedType.TypeApplication
            (SharedType.TypeApplication
              (SharedType.TypeConstructor identityName)
              (SharedType.TypeConstructor maybeName))
            (SharedType.TypeConstructor intName)
      TypeSynonym.expandTypeSynonyms freshStringVariable aliases
          overapplied @?=
        Right (SharedType.TypeApplication
          (SharedType.TypeConstructor maybeName)
          (SharedType.TypeConstructor intName))
      TypeSynonym.expandTypeSynonyms freshStringVariable aliases
          (SharedType.TypeConstructor identityName) @?=
        Left (TypeSynonym.UnsaturatedTypeSynonym identityName 1 0)
  , testCase "elaborate empty, singleton, and ordered type batches" $ do
      let identityName = right $ mkIdentifier "Identity"
          boolName = right $ mkIdentifier "Bool"
          intName = right $ mkIdentifier "Int"
          declarations =
            [ Declaration.TypeSynonymDeclaration () identityName
                [Declaration.TypeParameter "a" Nothing]
                $ SharedType.TypeVariable "a"
            , Declaration.AbstractTypeDeclaration () boolName synonymProper
            , Declaration.AbstractTypeDeclaration () intName synonymProper
            ]
          aliases = preparedSynonyms declarations
          applyIdentity argument = SharedType.TypeApplication
            (SharedType.TypeConstructor identityName) argument
          boolType = SharedType.TypeConstructor boolName
          intType = SharedType.TypeConstructor intName
          boolAlias = applyIdentity boolType
          intAlias = applyIdentity intType
          emptyBatch = [] ::
            [(KindInference.GroundKind, SharedType.Type String)]
      TypeSynonym.elaborateTypes freshStringVariable aliases emptyBatch @?=
        Right []
      TypeSynonym.elaborateTypes freshStringVariable aliases
          [(synonymProper, boolAlias)] @?=
        (: []) <$> TypeSynonym.elaborateType freshStringVariable aliases
          synonymProper boolAlias
      TypeSynonym.elaborateTypes freshStringVariable aliases
          [(synonymProper, boolAlias), (synonymProper, intAlias)] @?=
        Right [boolType, intType]
  , testCase "share free-variable kinds across an elaboration batch" $ do
      let boolName = right $ mkIdentifier "Bool"
          boolType = SharedType.TypeConstructor boolName
          aliases = preparedSynonyms
            [Declaration.AbstractTypeDeclaration () boolName synonymProper]
          shared = SharedType.TypeVariable "shared"
          applied = SharedType.TypeApplication shared boolType
      -- Either member is independently kindable. Together they would assign
      -- incompatible proper and function kinds to the same free variable.
      case TypeSynonym.elaborateTypes freshStringVariable aliases
          [(synonymProper, shared), (synonymProper, applied)] of
        Left (TypeSynonym.IllKindedType TypeSynonym.BeforeExpansion _) ->
          pure ()
        result -> fail $ "batch kind scope was split: " ++ show result
  , testCase "freshen forall binders before parameter substitution" $ do
      let captureName = right $ mkIdentifier "Capture"
          capture = Declaration.TypeSynonymDeclaration () captureName
            [Declaration.TypeParameter "p" Nothing]
            $ SharedType.ForallType ["q"] []
            $ SharedType.FunctionType
                (SharedType.TypeVariable "p")
                (SharedType.TypeVariable "q")
          aliases = preparedSynonyms [capture]
          applied = SharedType.TypeApplication
            (SharedType.TypeConstructor captureName)
            (SharedType.TypeVariable "q")
      TypeSynonym.expandTypeSynonyms freshStringVariable aliases applied @?=
        Right (SharedType.ForallType ["q'"] []
          $ SharedType.FunctionType
              (SharedType.TypeVariable "q")
              (SharedType.TypeVariable "q'"))
      TypeSynonym.expandTypeSynonyms
          (\_ binder -> Just binder) aliases applied @?=
        Left (TypeSynonym.FreshVariableCollision "q" "q")
  , testCase "raw definitions expand only reachable aliases safely" $ do
      let captureName = right $ mkIdentifier "Capture"
          cycleA = right $ mkIdentifier "CycleA"
          cycleB = right $ mkIdentifier "CycleB"
          definitions = Map.fromList
            [ ( captureName
              , ( ["p"]
                , SharedType.ForallType ["q"] []
                  $ SharedType.FunctionType
                      (SharedType.TypeVariable "p")
                      (SharedType.TypeVariable "q")
                )
              )
            , (cycleA, ([], SharedType.TypeConstructor cycleB))
            , (cycleB, ([], SharedType.TypeConstructor cycleA))
            ]
          applied = SharedType.TypeApplication
            (SharedType.TypeConstructor captureName)
            (SharedType.TypeVariable "q")
      TypeSynonym.expandTypeSynonymDefinitions
          freshStringVariable definitions applied @?=
        Right (SharedType.ForallType ["q'"] []
          $ SharedType.FunctionType
              (SharedType.TypeVariable "q")
              (SharedType.TypeVariable "q'"))
  , testCase "substitute parameters simultaneously" $ do
      let swapName = right $ mkIdentifier "Swap"
          swap = Declaration.TypeSynonymDeclaration () swapName
            [ Declaration.TypeParameter "a" Nothing
            , Declaration.TypeParameter "b" Nothing
            ]
            $ SharedType.TupleType Boxed
                [ SharedType.TypeVariable "a"
                , SharedType.TypeVariable "b"
                ]
          aliases = preparedSynonyms [swap]
          applied = SharedType.TypeApplication
            (SharedType.TypeApplication
              (SharedType.TypeConstructor swapName)
              (SharedType.TypeVariable "b"))
            (SharedType.TypeVariable "a")
      TypeSynonym.expandTypeSynonyms freshStringVariable aliases applied @?=
        Right (SharedType.TupleType Boxed
          [SharedType.TypeVariable "b", SharedType.TypeVariable "a"])
  , testCase "avoid capture in forall constraints and nested scopes" $ do
      let captureName = right $ mkIdentifier "NestedCapture"
          className = right $ mkIdentifier "C"
          capture = Declaration.TypeSynonymDeclaration () captureName
            [Declaration.TypeParameter "p" Nothing]
            $ SharedType.ForallType ["q"]
                [Constraint className [SharedType.TypeVariable "p"]]
            $ SharedType.TupleType Boxed
                [ SharedType.FunctionType
                    (SharedType.TypeVariable "p")
                    (SharedType.TypeVariable "q")
                , SharedType.ForallType ["q"] []
                    $ SharedType.FunctionType
                        (SharedType.TypeVariable "p")
                        (SharedType.TypeVariable "q")
                ]
          typeClass = Declaration.ClassDeclaration () className
            [Declaration.TypeParameter "c" Nothing] [] []
          aliases = preparedSynonyms [typeClass, capture]
          applied = SharedType.TypeApplication
            (SharedType.TypeConstructor captureName)
            (SharedType.TypeVariable "q")
      TypeSynonym.expandTypeSynonyms freshStringVariable aliases applied @?=
        Right (SharedType.ForallType ["q'"]
          [Constraint className [SharedType.TypeVariable "q"]]
          $ SharedType.TupleType Boxed
              [ SharedType.FunctionType
                  (SharedType.TypeVariable "q")
                  (SharedType.TypeVariable "q'")
              , SharedType.ForallType ["q''"] []
                  $ SharedType.FunctionType
                      (SharedType.TypeVariable "q")
                      (SharedType.TypeVariable "q''")
              ])
  , testCase "do not freshen binders for irrelevant substitutions" $ do
      let constantName = right $ mkIdentifier "Constant"
          intName = right $ mkIdentifier "Int"
          constant = Declaration.TypeSynonymDeclaration () constantName
            [Declaration.TypeParameter "p" Nothing]
            $ SharedType.ForallType ["q"] []
            $ SharedType.TypeConstructor intName
          aliases = preparedSynonyms
            [ constant
            , Declaration.AbstractTypeDeclaration () intName synonymProper
            ]
          applied = SharedType.TypeApplication
            (SharedType.TypeConstructor constantName)
            (SharedType.TypeVariable "q")
      TypeSynonym.expandTypeSynonyms
          (\_ _ -> Nothing) aliases applied @?=
        Right (SharedType.ForallType ["q"] []
          $ SharedType.TypeConstructor intName)
  , testCase "kind-check phantom arguments before expansion" $ do
      let phantomName = right $ mkIdentifier "Phantom"
          intName = right $ mkIdentifier "Int"
          boolName = right $ mkIdentifier "Bool"
          higherKind = Kind.FunctionKind synonymProper synonymProper
          declarations =
            [ Declaration.TypeSynonymDeclaration () phantomName
                [Declaration.TypeParameter "f" $ Just higherKind]
                $ SharedType.TypeConstructor intName
            , Declaration.AbstractTypeDeclaration () intName synonymProper
            , Declaration.AbstractTypeDeclaration () boolName synonymProper
            ]
          aliases = preparedSynonyms declarations
          malformed = SharedType.TypeApplication
            (SharedType.TypeConstructor phantomName)
            (SharedType.TypeConstructor boolName)
      case TypeSynonym.elaborateType freshStringVariable aliases
          synonymProper malformed of
        Left (TypeSynonym.IllKindedType TypeSynonym.BeforeExpansion _) ->
          pure ()
        result -> fail $ "phantom argument escaped pre-expansion checking: "
          ++ show result
  , testCase "reject an unused definition containing a partial alias" $ do
      let identityName = right $ mkIdentifier "Identity"
          higherName = right $ mkIdentifier "Higher"
          badName = right $ mkIdentifier "Bad"
          higherKind = Kind.FunctionKind synonymProper synonymProper
          declarations =
            [ Declaration.TypeSynonymDeclaration () identityName
                [Declaration.TypeParameter "a" Nothing]
                $ SharedType.TypeVariable "a"
            , Declaration.AbstractTypeDeclaration () higherName
                $ Kind.FunctionKind higherKind synonymProper
            , Declaration.TypeSynonymDeclaration () badName []
                $ SharedType.TypeApplication
                    (SharedType.TypeConstructor higherName)
                    (SharedType.TypeConstructor identityName)
            ]
          inventory = right $ Inventory.mkInventory
            KindInference.OpenKindInventory declarations
      TypeSynonym.prepareTypeSynonyms freshStringVariable inventory @?=
        Left (TypeSynonym.UnsaturatedTypeSynonym identityName 1 0)
  , testCase "reject aliases that compete with intrinsic constructors" $ do
      let declaration :: Declaration.Declaration String Void ()
          declaration = Declaration.TypeSynonymDeclaration () functionName
            [ Declaration.TypeParameter "a" Nothing
            , Declaration.TypeParameter "b" Nothing
            ]
            $ SharedType.FunctionType
                (SharedType.TypeVariable "a")
                (SharedType.TypeVariable "b")
          inventory = right $ Inventory.mkInventory
            KindInference.OpenKindInventory [declaration]
      TypeSynonym.prepareTypeSynonyms freshStringVariable inventory @?=
        Left (TypeSynonym.IntrinsicTypeSynonym functionName)
  , testCase "expand exact qualified aliases throughout declarations" $ do
      let moduleA = right $ mkModuleName "A"
          moduleB = right $ mkModuleName "B"
          aliasA = right $ mkQualifiedIdentifier moduleA "T"
          aliasB = right $ mkQualifiedIdentifier moduleB "T"
          intName = right $ mkIdentifier "Int"
          boolName = right $ mkIdentifier "Bool"
          valueName = right $ mkIdentifier "convert"
          declarations =
            [ Declaration.TypeSynonymDeclaration () aliasA []
                $ SharedType.TypeConstructor intName
            , Declaration.TypeSynonymDeclaration () aliasB []
                $ SharedType.TypeConstructor boolName
            , Declaration.AbstractTypeDeclaration () intName synonymProper
            , Declaration.AbstractTypeDeclaration () boolName synonymProper
            ]
          aliases = preparedSynonyms declarations
          valueDeclaration
            :: Declaration.Declaration String Void ()
          valueDeclaration = Declaration.ValueDeclaration
            $ Declaration.ValueSignature () valueName
            $ SharedType.FunctionType
                (SharedType.TypeConstructor aliasA)
                (SharedType.TypeConstructor aliasB)
      TypeSynonym.expandDeclarationTypeSynonyms freshStringVariable aliases
          valueDeclaration @?=
        Right (Declaration.ValueDeclaration
          $ Declaration.ValueSignature () valueName
          $ SharedType.FunctionType
              (SharedType.TypeConstructor intName)
              (SharedType.TypeConstructor boolName))
  ]

preparedSynonyms
  :: [Declaration.Declaration String Void ()]
  -> TypeSynonym.TypeSynonyms String
preparedSynonyms declarations = right
  $ TypeSynonym.prepareTypeSynonyms freshStringVariable
  $ right
  $ Inventory.mkInventory KindInference.OpenKindInventory declarations

freshStringVariable
  :: Set.Set String
  -> String
  -> Maybe String
freshStringVariable reserved = Just . choose . (++ "'")
 where
  choose candidate
    | candidate `Set.member` reserved = choose $ candidate ++ "'"
    | otherwise = candidate

synonymProper :: KindInference.GroundKind
synonymProper = Kind.ProperTypeKind

searchTests :: TestTree
searchTests = testGroup "search status"
  [ testCase "finished and truncated completions stay distinct" $ do
      Completed Finished @?= Completed Finished
      truncated StepLimitReached @?=
        Truncated (StepLimitReached :| [])
  , testCase "classify every optional progress state without losing reasons" $ do
      observeProgress Nothing @?= NoProgressObserved
      observeProgress (Just Continuing) @?= ObservedContinuing
      observeProgress (Just $ Completed Finished) @?= ObservedFinished
      let reasons = QueueLimitPruned 7 :| [DepthLimitPruned 2]
          observed = ObservedTruncated reasons
      observeProgress (Just $ Completed $ Truncated reasons) @?= observed
      _ <- evaluate $ force observed
      pure ()
  , testCase "truncation retains every independent pruning reason" $ do
      let completion = Truncated
            (QueueLimitPruned 7 :| [DepthLimitPruned 2])
      _ <- evaluate $ force completion
      completion @?= Truncated
        (QueueLimitPruned 7 :| [DepthLimitPruned 2])
  , testCase "batch functor changes candidates only" $
      fmap (+ 1) (SearchBatch Continuing "stats" [1 :: Int, 2]) @?=
        SearchBatch Continuing "stats" [2, 3]
  , testCase "query evidence remains independent of completion" $ do
      let finishedWithoutEvidence = right $ mkQueryResult NoEvidence
            $ SearchBatch (Completed Finished) "metadata" ([] :: [Int])
          truncatedWithCandidate = queryResultFromCandidates $ SearchBatch
            (Completed $ truncated CandidateLimitReached)
            "metadata"
            [1 :: Int]
          mappedExpected = queryResultFromCandidates $ SearchBatch
            (Completed $ truncated CandidateLimitReached)
            "metadata"
            [2 :: Int]
      resultEvidence finishedWithoutEvidence @?= NoEvidence
      resultEvidence truncatedWithCandidate @?= ValidatedCandidates
      fmap (+ 1) truncatedWithCandidate @?= mappedExpected
      _ <- evaluate $ force truncatedWithCandidate
      pure ()
  , testCase "check every evidence and candidate-presence combination" $ do
      let emptyBatch = SearchBatch Continuing "metadata" ([] :: [Int])
          nonemptyBatch = SearchBatch Continuing "metadata" [1 :: Int]
          observe result = (resultEvidence result, resultSearch result)
      forM_ [minBound .. maxBound] $ \evidence -> do
        fmap observe (mkQueryResult evidence emptyBatch) @?=
          if evidence == ValidatedCandidates
            then Left EmptyValidatedCandidates
            else Right (evidence, emptyBatch)
        fmap observe (mkQueryResult evidence nonemptyBatch) @?=
          if evidence == ValidatedCandidates
            then Right (evidence, nonemptyBatch)
            else Left $ CandidatesWithoutValidatedEvidence evidence
  , testCase "checked construction leaves a candidate tail lazy" $ do
      let batch = SearchBatch Continuing ()
            (1 : error "mkQueryResult forced the candidate tail")
          result = right $ mkQueryResult ValidatedCandidates batch
      resultEvidence result @?= ValidatedCandidates
      take 1 (batchCandidates $ resultSearch result) @?= [1 :: Int]
      mkQueryResult NoEvidence batch @?=
        Left (CandidatesWithoutValidatedEvidence NoEvidence)
  , testCase "derived construction leaves a candidate tail lazy" $ do
      let batch = SearchBatch Continuing ()
            (1 : error "queryResultFromCandidates forced the candidate tail")
          result = queryResultFromCandidates batch
      resultEvidence result @?= ValidatedCandidates
      take 1 (batchCandidates $ resultSearch result) @?= [1 :: Int]
  ]

selectionTests :: TestTree
selectionTests = testGroup "result selection"
  [ testCase "first skips inadmissible candidates and leaves the suffix lazy" $ do
      let results =
            [ queryResult Continuing [2 :: Int, 3]
            , error "SelectFirst forced the uninspected result suffix"
            ]
          selection = selectQueryResults SelectFirst
            (\_ -> error "SelectFirst evaluated its unused rank function" :: Int)
            odd results
      selection @?= Selection (Just Continuing) [3]
      observeProgress (selectionProgress selection) @?= ObservedContinuing
  , testCase "all preserves order and streams without forcing final progress" $ do
      let streaming = selectQueryResults SelectAll
            (\_ -> error "SelectAll evaluated its unused rank function" :: Int)
            odd
            [ queryResult Continuing [1 :: Int, 2, 3]
            , error "SelectAll forced the result suffix while streaming"
            ]
      take 2 (selectionCandidates streaming) @?= [1, 3]

      let terminal = Completed $ truncated StepLimitReached
          complete = selectQueryResults SelectAll id odd
            [ queryResult Continuing [1 :: Int, 2]
            , queryResult Continuing [3, 4]
            , queryResult terminal []
            ]
      complete @?= Selection (Just terminal) [1, 3]
  , testCase "best keeps every globally minimal admissible candidate" $ do
      let terminal = Completed $ truncated CandidateLimitReached
          results =
            [ queryResult Continuing
                [(3 :: Int, "superseded-first"), (3, "superseded-second")]
            , queryResult Continuing [(2, "inadmissible"), (1, "first")]
            , queryResult Continuing [(2, "worse"), (1, "second")]
            , queryResult terminal []
            ]
          selection = selectQueryResults SelectBest fst
            ((/= "inadmissible") . snd) results
      selection @?= Selection (Just terminal)
        [(1, "first"), (1, "second")]
  , testCase "lookahead resets on improvement and counts later batches" $ do
      let terminal = Completed Finished
          results =
            [ queryResult Continuing
                [(8 :: Int, "worse"), (5, "old-first"), (5, "old-second")]
            , queryResult Continuing [(6, "no improvement")]
            , queryResult Continuing [(4, "new-first"), (4, "new-second")]
            , queryResult Continuing [(4, "equal"), (9, "worse again")]
            , queryResult terminal []
            , error "lookahead inspected a batch beyond its exhausted budget"
            ]
          selection = selectQueryResults (SelectBestLookahead 2)
            fst (const True) results
      selection @?= Selection (Just terminal)
        [(4, "new-first"), (4, "new-second"), (4, "equal")]
  , testCase "non-positive lookahead stops at the first admissible batch" $ do
      let selection = selectQueryResults (SelectBestLookahead 0) id odd
            [ queryResult Continuing [2 :: Int]
            , queryResult (Completed Finished) [5, 3, 3]
            , error "zero lookahead forced a later batch"
            ]
      selection @?= Selection (Just $ Completed Finished) [3, 3]
  , testCase "preferred lookahead discards fallback and counts batches" $ do
      let fallback = False
          preferred = True
          terminal = Completed $ truncated StepLimitReached
          results =
            [ queryResult Continuing
                [(0 :: Int, fallback, "better-ranked fallback")]
            , queryResult Continuing [(10, preferred, "initial preferred")]
            -- Several worse candidates still spend only one batch unit, so
            -- the improvement in the next batch remains visible.
            , queryResult Continuing
                [ (11, preferred, "worse one")
                , (12, preferred, "worse two")
                , (-1, fallback, "fallback after preferred")
                ]
            , queryResult Continuing [(9, preferred, "improved")]
            , queryResult Continuing [(9, preferred, "equal")]
            , queryResult terminal
                [(20, preferred, "worse three"), (30, preferred, "worse four")]
            , error "preferred lookahead inspected beyond its batch budget"
            ]
          selection = selectPreferredQueryResults 2
            (\(rank, _, _) -> rank)
            (const True)
            (\(_, isPreferred, _) -> isPreferred)
            results
      selection @?= Selection (Just terminal)
        [(9, preferred, "improved"), (9, preferred, "equal")]
  , testCase "preferred lookahead returns best fallback when none appears" $ do
      let terminal = Completed Finished
          results =
            [ queryResult Continuing
                [(3 :: Int, "older-worse"), (1, "first-best")]
            , queryResult Continuing
                [(1, "second-best"), (0, "inadmissible")]
            , queryResult terminal []
            ]
          selection = selectPreferredQueryResults 2 fst
            ((/= "inadmissible") . snd)
            (const False)
            results
      selection @?= Selection (Just terminal)
        [(1, "first-best"), (1, "second-best")]
  , testCase "empty selections retain the last inspected progress" $ do
      let terminal = Completed Finished
          results =
            [ queryResult Continuing [2 :: Int]
            , queryResult terminal []
            ]
          modes =
            [ SelectFirst
            , SelectBest
            , SelectBestLookahead 1
            , SelectAll
            ]
      forM_ modes $ \mode ->
        selectQueryResults mode id odd results @?=
          Selection (Just terminal) []
      selectQueryResults SelectBest id odd [] @?=
        Selection Nothing ([] :: [Int])
  ]
 where
  queryResult progress candidates = queryResultFromCandidates
    $ SearchBatch progress () candidates

generatedTests :: TestTree
generatedTests = testGroup "generated syntax"
  [ testCase "check generated-definition names once" $ do
      let namespace = right $ mkModuleName "Fixture"
          identifier = right $ mkIdentifier "result"
          operator = right $ mkOperator "+"
          qualifiedIdentifier = right
            $ mkQualifiedIdentifier namespace "result"
          qualifiedOperator = right $ mkQualifiedOperator namespace "+"
          constructorIdentifier = right $ mkIdentifier "Result"
          constructorOperator = right $ mkOperator ":+"
          acceptedIdentifier = right $ mkDefinitionName identifier
          acceptedOperator = right $ mkDefinitionName operator
          checkedClause :: FunctionClause Int
          checkedClause = FunctionClause acceptedIdentifier [] $ Tuple []
          checkedOperatorClause :: FunctionClause Int
          checkedOperatorClause = FunctionClause acceptedOperator [] $ Tuple []
          rejected name = mkDefinitionName name @?=
            Left (InvalidFunctionName name)
          query :: QueryRequest String ()
          query = QueryRequest acceptedIdentifier "goal" [] ()
      definitionName acceptedIdentifier @?= identifier
      definitionSpelling acceptedIdentifier @?= "result"
      definitionName acceptedOperator @?= operator
      definitionSpelling acceptedOperator @?= "+"
      show acceptedIdentifier @?= show identifier
      clauseName checkedClause @?= acceptedIdentifier
      renderFunctionClause (defaultRenderOptions show)
          checkedOperatorClause @?= Right "(+) = ()"
      show query @?=
        "QueryRequest {requestTarget = result, requestGoal = \"goal\", \
        \requestContexts = [], requestOptions = ()}"
      validateDefinitionName identifier @?= Right ()
      validateDefinitionName qualifiedIdentifier @?=
        Left (InvalidFunctionName qualifiedIdentifier)
      rejected qualifiedIdentifier
      rejected qualifiedOperator
      rejected constructorIdentifier
      rejected constructorOperator
      rejected listName
      rejected consName
      rejected functionName
      rejected $ right $ tupleName Boxed 0
      -- The structural Name API rejects @_@ even before this narrower
      -- definition boundary, so clients cannot manufacture that case.
      mkIdentifier "_" @?= Left (ReservedIdentifier "_")
      _ <- evaluate $ force acceptedIdentifier
      _ <- evaluate $ force query
      pure ()
  , testCase "validate lambda, let, and case scopes" $ do
      let just = right $ mkIdentifier "Just"
          expression = Lambda [Bind (0 :: Int)] $
            Let (Bind 1) (Local 0) $
              Case (Local 1)
                [(Constructor just [Bind 2], Local 2)]
      validateExpressionScope expression @?= Right ()
  , testCase "reject free locals and duplicate pattern binders" $ do
      validateExpressionScope (Local (0 :: Int)) @?=
        Left (UnboundLocal 0)
      validateExpressionScope
          (Lambda [TuplePattern [Bind (0 :: Int), Bind 0]] $ Local 0)
        @?= Left (DuplicatePatternBinder 0)
      validateExpressionScope
          (Lambda [Bind (0 :: Int)] $ Lambda [Bind 0] $ Local 0)
        @?= Left (DuplicatePatternBinder 0)
  , testCase "map, fold, traverse, and force every local occurrence" $ do
      let expression = Lambda [Bind (1 :: Int)] (Local 1)
      fmap (+ 10) expression @?= Lambda [Bind 11] (Local 11)
      sum expression @?= 2
      traverse (\local -> if local > 0 then Just (show local) else Nothing)
          expression
        @?= Just (Lambda [Bind "1"] (Local "1"))
      _ <- evaluate $ force expression
      pure ()
  , testCase "allocate locals against globals and explicit reservations" $ do
      let namespace = right $ mkModuleName "M"
          globalA = right $ mkQualifiedIdentifier namespace "a"
          expression = Lambda [Bind (1 :: Int), Bind 2] $
            Apply (Local 1) (Global globalA)
          unqualified = RenderOptions Unqualified (const "a") []
          qualified = RenderOptions FullyQualified (const "a") []
          reserved = RenderOptions FullyQualified (const "a") ["a"]
      allocateLocalNames unqualified expression @?=
        Right (Map.fromList [(1, "a'"), (2, "a''")])
      allocateLocalNames qualified expression @?=
        Right (Map.fromList [(1, "a"), (2, "a'")])
      allocateLocalNames reserved expression @?=
        Right (Map.fromList [(1, "a'"), (2, "a''")])
  , testCase "allocate emitted hole spellings without collisions" $ do
      let globalUnderscoreA = right $ mkIdentifier "_a"
          options reservations =
            RenderOptions Unqualified id reservations
          globalCollision :: Expression String
          globalCollision = Apply (Hole "a") (Global globalUnderscoreA)
          mixedLocals :: Expression String
          mixedLocals = Tuple [Local "a", Local "_a", Hole "a"]
          unaffected :: Expression String
          unaffected = Tuple [Local "a", Local "_a"]
      allocateLocalNames (options []) globalCollision @?=
        Right (Map.fromList [("a", "a'")])
      renderExpression (options []) globalCollision @?= Right "_a' _a"
      allocateLocalNames (options ["_a"]) (Hole "a") @?=
        Right (Map.fromList [("a", "a'")])
      renderExpression (options ["_a"]) (Hole "a") @?= Right "_a'"
      allocateLocalNames (options []) mixedLocals @?=
        Right (Map.fromList [("a", "a"), ("_a", "_a'")])
      renderExpression (options []) mixedLocals @?=
        Right "(a, _a', _a)"
      allocateLocalNames (options []) unaffected @?=
        Right (Map.fromList [("a", "a"), ("_a", "_a")])
  , testCase "render lambdas, tuples, and symbolic applications" $ do
      let plus = right $ mkOperator "+"
          true = right $ mkIdentifier "True"
          false = right $ mkIdentifier "False"
          expression = Lambda [Bind (0 :: Int)] $
            Apply (Apply (Global plus) (Local 0)) $
              Tuple [Global true, Global false]
      renderExpression (defaultRenderOptions (const "x")) expression @?=
        Right "\\x -> x + (True, False)"
  , testCase "render an empty case with explicit braces" $ do
      let eliminate = right $ mkIdentifier "eliminate"
          checkedEliminate = right $ mkDefinitionName eliminate
          clause = FunctionClause checkedEliminate [Bind (0 :: Int)] $
            Case (Local 0) []
          options = defaultRenderOptions $ const "emptyValue"
      validateFunctionClauseScope clause @?= Right ()
      validateFunctionClauseSyntax FullyQualified clause @?= Right ()
      renderFunctionClause options clause @?=
        Right "eliminate emptyValue = case emptyValue of {}"
  , testCase "apply qualification consistently to identifiers and operators" $ do
      let namespace = right $ mkModuleName "Data.List"
          mapping = right $ mkQualifiedIdentifier namespace "map"
          append = right $ mkQualifiedOperator namespace "++"
          identifiers :: Expression Int
          identifiers = Apply (Global mapping) (Global mapping)
          operators :: Expression Int
          operators = Apply (Apply (Global append) (Global mapping))
            (Global mapping)
          options :: Qualification -> RenderOptions Int
          options qualification = RenderOptions qualification show []
      renderExpression (options Unqualified) identifiers @?=
        Right "map map"
      renderExpression (options QualifyIdentifiers) identifiers @?=
        Right "Data.List.map Data.List.map"
      renderExpression (options QualifyIdentifiers) operators @?=
        Right "Data.List.map ++ Data.List.map"
      renderExpression (options FullyQualified) operators @?=
        Right "Data.List.map Data.List.++ Data.List.map"
  , testCase "render function clauses with scoped case patterns" $ do
      let select = right $ mkIdentifier "select"
          checkedSelect = right $ mkDefinitionName select
          just = right $ mkIdentifier "Just"
          nothing = right $ mkIdentifier "Nothing"
          clause = FunctionClause checkedSelect [Bind (0 :: Int)] $
            Case (Local 0)
              [ (Constructor just [Bind 1], Local 1)
              , (Constructor nothing [], Local 0)
              ]
          options = RenderOptions FullyQualified (const "select") []
      validateFunctionClauseScope clause @?= Right ()
      validateFunctionClauseSyntax FullyQualified clause @?= Right ()
      renderFunctionClause options clause @?=
        Right (unlinesWithoutFinal
          [ "select select' ="
          , "  case select' of"
          , "  Just select'' -> select''"
          , "  Nothing -> select'"
          ])
  , testCase "recover clause expressions without discarding binders" $ do
      let target = right $ mkIdentifier "target"
          checkedTarget = right $ mkDefinitionName target
          body = Local (0 :: Int)
          valueClause = FunctionClause checkedTarget [] body
          functionClause = FunctionClause checkedTarget [Bind 0, Wildcard] body
      functionClauseExpression valueClause @?= body
      functionClauseExpression functionClause @?=
        Lambda [Bind 0, Wildcard] body
  , testCase "validate every checked function-clause syntax layer" $ do
      let target = right $ mkIdentifier "target"
          checkedTarget = right $ mkDefinitionName target
          constructorName = right $ mkIdentifier "Just"
          variableName = right $ mkIdentifier "value"
          unit = Tuple [] :: Expression Int
      mkDefinitionName constructorName @?=
        Left (InvalidFunctionName constructorName)
      validateFunctionClauseSyntax FullyQualified
          (FunctionClause checkedTarget [Constructor variableName []] unit) @?=
        Left (InvalidConstructorPattern variableName)
      validateFunctionClauseSyntax FullyQualified
          (FunctionClause checkedTarget [] $ Lambda [] unit) @?= Left EmptyLambda
  , testCase "reject qualification that captures a definition global" $ do
      let target = right $ mkIdentifier "result"
          checkedTarget = right $ mkDefinitionName target
          namespace = right $ mkModuleName "Fixture"
          global = right $ mkQualifiedIdentifier namespace "result"
          clause = FunctionClause checkedTarget []
            (Global global :: Expression Int)
          options qualification = RenderOptions qualification show []
      validateFunctionClauseSyntax Unqualified clause @?=
        Left (GlobalDefinitionCapture target global Unqualified)
      validateFunctionClauseSyntax QualifyIdentifiers clause @?= Right ()
      validateFunctionClauseSyntax FullyQualified clause @?= Right ()
      renderFunctionClause (options Unqualified) clause @?=
        Left (GlobalDefinitionCapture target global Unqualified)
      renderFunctionClause (options QualifyIdentifiers) clause @?=
        Right "result = Fixture.result"
      renderFunctionClause (options FullyQualified) clause @?=
        Right "result = Fixture.result"
  , testCase "measure generated expressions structurally" $ do
      let identity = Lambda [Bind (0 :: Int)] $ Local 0
          application = Apply identity identity
      expressionSize (Local (0 :: Int)) @?= 1
      expressionSize identity @?= 2
      expressionSize application @?= 5
  , testCase "render holes and reject malformed surface shapes" $ do
      let options = defaultRenderOptions (\local -> 't' : show (local :: Int))
          variableName = right $ mkIdentifier "value"
      renderExpression options (Hole 3) @?= Right "_t3"
      validateExpressionSyntax
          (Global functionName :: Expression Int) @?=
        Left (InvalidGlobalExpression functionName)
      renderExpression options (Tuple [Global variableName]) @?=
        Left (InvalidTupleExpressionArity 1)
      renderExpression options
          (Lambda [Constructor variableName []] $ Global variableName)
        @?= Left (InvalidConstructorPattern variableName)
      renderExpression options
          (Lambda [Constructor consName [Bind 0]] $ Local 0)
        @?= Left (InvalidConstructorPatternArity consName 2 1)
      renderExpression (defaultRenderOptions $ const "case")
          (Hole (0 :: Int)) @?=
        Left (InvalidLocalName "case" $ ReservedIdentifier "case")
  ]

unlinesWithoutFinal :: [String] -> String
unlinesWithoutFinal = intercalate "\n"

constraintTests :: TestTree
constraintTests = testGroup "constraints"
  [ testCase "retain nominal class identity and argument arity" $ do
      let equality = right $ mkIdentifier "Eq"
          constraint = Constraint equality ["a", "b"]
      constraintClass constraint @?= equality
      constraintArguments constraint @?= ["a", "b"]
      constraintArity constraint @?= 2
      show constraint @?= "Eq \"a\" \"b\""
      show (Constraint equality [] :: Constraint ()) @?= "Eq"
  , testCase "qualified classes remain nominally distinct" $ do
      let moduleA = right $ mkModuleName "A"
          moduleB = right $ mkModuleName "B"
          classA = right $ mkQualifiedIdentifier moduleA "C"
          classB = right $ mkQualifiedIdentifier moduleB "C"
      Constraint classA [()] @?= Constraint classA [()]
      assertBool "different qualified classes compared equal"
        $ Constraint classA [()] /= Constraint classB [()]
  , testCase "validate the ordinary constructor class namespace" $ do
      let namespace = right $ mkModuleName "M"
          ordinary = right $ mkIdentifier "C"
          qualified = right $ mkQualifiedIdentifier namespace "C"
          operator = right $ mkOperator ":=>"
          lowercase = right $ mkIdentifier "c"
          valueOperator = right $ mkOperator "=="
          tuple = right $ tupleName Boxed 2
      mapM_ (\name -> validateConstraint (Constraint name [()]) @?= Right ())
        [ordinary, qualified, operator]
      mapM_ (\name -> validateConstraintClassName name @?=
          Left (InvalidConstraintClass name))
        [lowercase, valueOperator, tuple]
  , testCase "map, fold, and traverse affect only arguments" $ do
      let equality = right $ mkIdentifier "Eq"
          constraint = Constraint equality [1 :: Int, 2]
      fmap (+ 10) constraint @?= Constraint equality [11, 12]
      sum constraint @?= 3
      traverse (\value -> if value > 0 then Just (show value) else Nothing)
        constraint
        @?= Just (Constraint equality ["1", "2"])
  , testCase "deep evaluation reaches every argument" $ do
      let equality = right $ mkIdentifier "Eq"
      _ <- evaluate $ force $ Constraint equality [[1 :: Int], [2, 3]]
      pure ()
  ]

moduleTests :: TestTree
moduleTests = testGroup "module names"
  [ testCase "construct, render, and decompose" $ do
      let result = mkModuleName "Control.Monad.State"
      fmap moduleNameSegments result @?=
        Right ["Control", "Monad", "State"]
      fmap renderModuleName result @?= Right "Control.Monad.State"
      fmap renderModuleName
        (mkModuleNameSegments ["Data", "Map", "Strict"])
        @?= Right "Data.Map.Strict"
  , testCase "accept identifier characters in segments" $
      fmap renderModuleName (mkModuleName "A1.B_C.D'")
        @?= Right "A1.B_C.D'"
  , testCase "reject missing modules and segments" $ do
      mkModuleName "" @?= Left EmptyModuleName
      mkModuleNameSegments [] @?= Left EmptyModuleName
      mkModuleName "Data..List" @?= Left (InvalidModuleSegment "")
      mkModuleName ".Data" @?= Left (InvalidModuleSegment "")
      mkModuleName "Data." @?= Left (InvalidModuleSegment "")
  , testCase "reject lowercase and malformed segments" $ do
      mkModuleName "data.List" @?= Left (InvalidModuleSegment "data")
      mkModuleName "Data.list" @?= Left (InvalidModuleSegment "list")
      mkModuleName "Data.List-Extra" @?=
        Left (InvalidModuleSegment "List-Extra")
  ]

ordinaryTests :: TestTree
ordinaryTests = testGroup "ordinary names"
  [ testCase "classify identifiers without re-lexing" $ do
      let variable = right (mkIdentifier "foldl'")
          constructor = right (mkIdentifier "Just")
      nameOccurrence variable @?=
        IdentifierOccurrence VariableLike "foldl'"
      nameOccurrence constructor @?=
        IdentifierOccurrence ConstructorLike "Just"
      nameLexicalClass variable @?= VariableLike
      nameLexicalClass constructor @?= ConstructorLike
  , testCase "accept valid identifiers" $
      forM_ ["x", "_x", "foldl'", "x2", "Just", "Maybe"] $ \spelling ->
        assertBool spelling (not (isLeft (mkIdentifier spelling)))
  , testCase "reject all reserved identifiers" $
      forM_ reservedIdentifiers $ \spelling ->
        mkIdentifier spelling @?= Left (ReservedIdentifier spelling)
  , testCase "reject malformed identifiers" $ do
      mkIdentifier "" @?= Left EmptyName
      mkIdentifier "2fast" @?= Left (InvalidIdentifier "2fast")
      mkIdentifier "Data.map" @?= Left (InvalidIdentifier "Data.map")
      mkIdentifier "has space" @?= Left (InvalidIdentifier "has space")
  , testCase "classify operators without re-lexing" $ do
      nameOccurrence (right (mkOperator "<*>")) @?=
        OperatorOccurrence VariableLike "<*>"
      nameOccurrence (right (mkOperator ":+:")) @?=
        OperatorOccurrence ConstructorLike ":+:"
  , testCase "expose the lexical character predicates used by constructors" $ do
      assertBool "valid identifier continuation characters"
        $ all isIdentifierCharacter "aZ0_'\955"
      assertBool "invalid identifier continuation characters"
        $ all (not . isIdentifierCharacter) ".- \8853"
      assertBool "valid operator characters"
        $ all isOperatorCharacter "!#$%&*+./<=>?@\\^|-~:\8853"
      assertBool "invalid operator characters"
        $ all (not . isOperatorCharacter) "a0_ (),;[]`{}\"'"
  , testCase "accept valid operators" $
      forM_ ["+", "++", ".", "<*>", ":+:", "--*", "⊕"] $ \spelling ->
        assertBool spelling (not (isLeft (mkOperator spelling)))
  , testCase "reject all reserved operators" $
      forM_ reservedOperators $ \spelling ->
        mkOperator spelling @?= Left (ReservedOperator spelling)
  , testCase "reject malformed operators" $ do
      mkOperator "" @?= Left EmptyName
      mkOperator "plus" @?= Left (InvalidOperator "plus")
      mkOperator "," @?= Left (InvalidOperator ",")
  , testCase "retain module and occurrence independently" $ do
      let qualifier = right (mkModuleName "Data.List")
          name = right (mkQualifiedIdentifier qualifier "map")
      nameModule name @?= Just qualifier
      nameOccurrence name @?=
        IdentifierOccurrence VariableLike "map"
      nameSpelling name @?= Just "map"
  , testCase "return bare ordinary spellings but not structural specials" $ do
      let qualifier = right (mkModuleName "Control.Applicative")
      nameSpelling (right $ mkIdentifier "foldl'") @?= Just "foldl'"
      nameSpelling (right $ mkQualifiedOperator qualifier "<*>") @?= Just "<*>"
      map nameSpelling [listName, consName, functionName,
        right (tupleName Boxed 3)] @?= replicate 4 Nothing
  , testCase "render every identifier context" $ do
      let qualifier = right (mkModuleName "Data.List")
          name = right (mkQualifiedIdentifier qualifier "map")
      renderCanonical name @?= "Data.List.map"
      renderPrefix name @?= "Data.List.map"
      renderInfix name @?= Right (backticked "Data.List.map")
  , testCase "render every operator context distinctly" $ do
      let qualifier = right (mkModuleName "Control.Applicative")
          name = right (mkQualifiedOperator qualifier "<*>")
      renderCanonical name @?= "Control.Applicative.(<*>)"
      renderPrefix name @?= "(Control.Applicative.<*>)"
      renderInfix name @?= Right "Control.Applicative.<*>"
  , testCase "qualified dot operator remains unambiguous" $ do
      let qualifier = right (mkModuleName "M")
          name = right (mkQualifiedOperator qualifier ".")
      renderCanonical name @?= "M.(.)"
      renderPrefix name @?= "(M..)"
      renderInfix name @?= Right "M.."
      parseName "M.(.)" @?= Right name
      parseName "(M..)" @?= Right name
      parseName "M.." @?= Right name
  ]

specialTests :: TestTree
specialTests = testGroup "special names"
  [ testCase "list is structural and prefix-only" $ do
      nameOccurrence listName @?= SpecialOccurrence ListConstructor
      nameLexicalClass listName @?= ConstructorLike
      renderCanonical listName @?= "[]"
      renderPrefix listName @?= "[]"
      renderInfix listName @?= Left (NameHasNoInfixForm ListConstructor)
  , testCase "cons is structural in both contexts" $ do
      nameSpecial consName @?= Just ConsConstructor
      renderCanonical consName @?= "(:)"
      renderInfix consName @?= Right ":"
  , testCase "function is structural in both contexts" $ do
      nameSpecial functionName @?= Just FunctionConstructor
      renderCanonical functionName @?= "(->)"
      renderInfix functionName @?= Right "->"
  , testCase "all representative boxed tuples" $ do
      assertTuple Boxed 0 "()"
      assertTuple Boxed 2 "(,)"
      assertTuple Boxed 3 "(,,)"
      assertTuple Boxed 8 "(,,,,,,,)"
  , testCase "all representative unboxed tuples" $ do
      assertTuple Unboxed 0 "(# #)"
      assertTuple Unboxed 2 "(#,#)"
      assertTuple Unboxed 3 "(#,,#)"
      assertTuple Unboxed 8 "(#,,,,,,,#)"
  , testCase "reject invalid boxed arities" $
      forM_ [-3, -1, 1, maximumTupleArity + 1, maxBound] $ \arity -> do
        tupleName Boxed arity @?= Left (InvalidTupleArity Boxed arity)
        specialName (TupleConstructor Boxed arity) @?=
          Left (InvalidTupleArity Boxed arity)
  , testCase "reject invalid unboxed arities" $
      forM_ [-3, -1, 1, maximumTupleArity + 1, maxBound] $ \arity -> do
        tupleName Unboxed arity @?= Left (InvalidTupleArity Unboxed arity)
        specialName (TupleConstructor Unboxed arity) @?=
          Left (InvalidTupleArity Unboxed arity)
  ]

parserTests :: TestTree
parserTests = testGroup "parser"
  [ testCase "parse identifier prefix and infix forms" $ do
      parseName "map" @?= mkIdentifier "map"
      parseName (backticked "map") @?= mkIdentifier "map"
      let qualifier = right (mkModuleName "Data.List")
          expected = mkQualifiedIdentifier qualifier "map"
      parseName "Data.List.map" @?= expected
      parseName (backticked "Data.List.map") @?= expected
  , testCase "parse operator canonical, prefix, and infix forms" $ do
      parseName "<*>" @?= mkOperator "<*>"
      parseName "(<*>)" @?= mkOperator "<*>"
      let qualifier = right (mkModuleName "Control.Applicative")
          expected = mkQualifiedOperator qualifier "<*>"
      parseName "Control.Applicative.(<*>)" @?= expected
      parseName "(Control.Applicative.<*>)" @?= expected
      parseName "Control.Applicative.<*>" @?= expected
  , testCase "parse all built-in forms" $ do
      parseName "[]" @?= Right listName
      parseName "(:)" @?= Right consName
      parseName ":" @?= Right consName
      parseName "(->)" @?= Right functionName
      parseName "->" @?= Right functionName
      parseName "()" @?= tupleName Boxed 0
      parseName "(,)" @?= tupleName Boxed 2
      parseName "(# #)" @?= tupleName Unboxed 0
      parseName "(#,#)" @?= tupleName Unboxed 2
      parseName "(##)" @?= mkOperator "##"
  , testCase "ignore outer and contextual whitespace" $ do
      parseName "  Data.List.map\n" @?= parseName "Data.List.map"
      parseName "( <*> )" @?= mkOperator "<*>"
      parseName (backticked " map ") @?= mkIdentifier "map"
  , testCase "preserve reserved-token errors" $ do
      parseName "case" @?= Left (ReservedIdentifier "case")
      parseName "=" @?= Left (ReservedOperator "=")
      parseName "(=)" @?= Left (ReservedOperator "=")
  , testCase "reject malformed and partial forms" $
      forM_ ["", "   ", "Data..map", "(map)", backticked "+",
             "(#,#,#)", "foo bar"] $ \source ->
        assertBool source (isLeft (parseName source))
  ]

diagnosticTests :: TestTree
diagnosticTests = testGroup "diagnostics"
  [ testCase "minimal diagnostic" $ do
      let value = diagnostic Error "cannot synthesize a term"
      diagnosticSeverity value @?= Error
      diagnosticCode value @?= Nothing
      diagnosticSource value @?= Nothing
      diagnosticSpan value @?= Nothing
      diagnosticContext value @?= []
      renderDiagnostic value @?= "error: cannot synthesize a term"
  , testCase "coded contextual diagnostic" $ do
      let value = contextualDiagnostic Warning "SYN002"
            "search used a fallback" "while checking Example.answer"
      diagnosticSeverity value @?= Warning
      diagnosticCode value @?= Just "SYN002"
      diagnosticContext value @?= ["while checking Example.answer"]
      renderDiagnostic value @?=
        "warning [SYN002]: search used a fallback\n\
        \  context: while checking Example.answer"
  , testCase "complete source location" $ do
      let span' = validSourceSpan 2 3 4 5
          value = withLocation "Example.hs" span'
            $ codedDiagnostic Error "SYN003" "cannot lower declaration"
      diagnosticSource value @?= Just "Example.hs"
      diagnosticSpan value @?= Just span'
      renderDiagnostic value @?=
        "Example.hs:2:3-4:5: error [SYN003]: cannot lower declaration"
  , testCase "code, source, span, and ordered context" $ do
      let span' = validSourceSpan 3 7 3 12
          value =
            withContext "while checking query a -> a" $
            withContext "in module Example" $
            withSpan span' $
            withSource "Example.hs" $
            withCode "SYN001" $
            diagnostic Error "unknown type constructor Foo"
      renderDiagnostic value @?=
        "Example.hs:3:7-12: error [SYN001]: unknown type constructor Foo\n\
        \  context: in module Example\n\
        \  context: while checking query a -> a"
  , testCase "multiline span without source" $ do
      let span' = validSourceSpan 4 2 6 9
      renderDiagnostic (withSpan span' (diagnostic Warning "ambiguous name"))
        @?= "4:2-6:9: warning: ambiguous name"
  , testCase "point span and empty code" $ do
      let point = validSourcePosition 1 1
          value = withCode "" $ withSpan (right $ mkSourceSpan point point) $
            diagnostic Info "using default search budget"
      renderDiagnostic value @?= "1:1: info: using default search budget"
  , testCase "reject invalid positions and reversed spans" $ do
      mkSourcePosition 0 7 @?= Left (NonPositiveSourceLine 0)
      mkSourcePosition 3 (-1) @?= Left (NonPositiveSourceColumn (-1))
      let start = validSourcePosition 4 2
          end = validSourcePosition 3 9
      mkSourceSpan start end @?=
        Left (SourceSpanEndBeforeStart start end)
      let point = right $ mkSourceSpan start start
      sourceStart point @?= start
      sourceEnd point @?= start
  , testCase "complete text spans use one-based half-open positions" $ do
      sourceTextSpan "" @?=
        validSourceSpan 1 1 1 1
      sourceTextSpan "ab\nc" @?=
        validSourceSpan 1 1 2 2
      sourceTextSpan "ab\n" @?=
        validSourceSpan 1 1 2 1
  , testCase "total NFData instances" $ do
      _ <- evaluate (force (right (parseName "Data.List.(++)")))
      _ <- evaluate (force (withContext "query" (diagnostic Error "failure")))
      return ()
  , testCase "malformed public tuple errors render in bounded space" $ do
      renderNameError (NameHasNoInfixForm
          $ TupleConstructor Boxed minBound) @?=
        "name <invalid boxed tuple constructor arity "
          ++ show (minBound :: Int) ++ "> has no infix form"
      renderNameError (NameHasNoInfixForm
          $ TupleConstructor Unboxed maxBound) @?=
        "name <invalid unboxed tuple constructor arity "
          ++ show (maxBound :: Int) ++ "> has no infix form"
  ]

propertyTests :: TestTree
propertyTests = testGroup "round trips"
  [ QC.testProperty "canonical" $ \(ValidName name) ->
      parseName (renderCanonical name) QC.=== Right name
  , QC.testProperty "prefix" $ \(ValidName name) ->
      parseName (renderPrefix name) QC.=== Right name
  , QC.testProperty "infix where defined" $ \(ValidName name) ->
      case renderInfix name of
        Right rendered -> parseName rendered QC.=== Right name
        Left (NameHasNoInfixForm _) -> QC.property True
        Left unexpected -> QC.counterexample (show unexpected) False
  , QC.testProperty "Show is canonical" $ \(ValidName name) ->
      show name QC.=== renderCanonical name
  , QC.testProperty "occurrence accessors agree" $ \(ValidName name) ->
      accessorsAgree name
  , QC.testProperty "identifier continuation predicate agrees with construction" $
      \character ->
        not (isLeft $ mkIdentifier ("zz" ++ [character])) QC.===
          isIdentifierCharacter character
  , QC.testProperty "operator character predicate agrees with construction" $
      \character ->
        not (isLeft $ mkOperator ['+', character]) QC.===
          isOperatorCharacter character
  , QC.testProperty "module render/parse" $ \(ValidModule moduleName) ->
      mkModuleName (renderModuleName moduleName) QC.=== Right moduleName
  ]

assertTuple :: Boxity -> Int -> String -> Assertion
assertTuple boxity arity expected = do
  let name = right (tupleName boxity arity)
      builtIn = TupleConstructor boxity arity
  nameOccurrence name @?= SpecialOccurrence builtIn
  nameLexicalClass name @?= ConstructorLike
  renderCanonical name @?= expected
  renderPrefix name @?= expected
  renderInfix name @?= Left (NameHasNoInfixForm builtIn)
  parseName expected @?= Right name

accessorsAgree :: Name -> QC.Property
accessorsAgree name = case nameOccurrence name of
  IdentifierOccurrence lexicalClass spelling -> QC.conjoin
    [ nameSpelling name QC.=== Just spelling
    , nameIdentifier name QC.=== Just spelling
    , nameOperator name QC.=== Nothing
    , nameSpecial name QC.=== Nothing
    , nameLexicalClass name QC.=== lexicalClass
    ]
  OperatorOccurrence lexicalClass spelling -> QC.conjoin
    [ nameSpelling name QC.=== Just spelling
    , nameIdentifier name QC.=== Nothing
    , nameOperator name QC.=== Just spelling
    , nameSpecial name QC.=== Nothing
    , nameLexicalClass name QC.=== lexicalClass
    ]
  SpecialOccurrence builtIn -> QC.conjoin
    [ nameSpelling name QC.=== Nothing
    , nameIdentifier name QC.=== Nothing
    , nameOperator name QC.=== Nothing
    , nameSpecial name QC.=== Just builtIn
    , nameLexicalClass name QC.=== ConstructorLike
    ]

newtype ValidName = ValidName Name

instance Show ValidName where
  show (ValidName name) = renderCanonical name

instance QC.Arbitrary ValidName where
  arbitrary = ValidName <$> QC.frequency
    [ (4, genIdentifier VariableLike), (3, genIdentifier ConstructorLike)
    , (4, genOperator VariableLike), (3, genOperator ConstructorLike)
    , (2, QC.elements [listName, consName, functionName])
    , (2, genTuple Boxed), (2, genTuple Unboxed)
    ]
  shrink _ = []

newtype ValidModule = ValidModule ModuleName

instance Show ValidModule where
  show (ValidModule name) = renderModuleName name

instance QC.Arbitrary ValidModule where
  arbitrary = ValidModule <$> genModule
  shrink _ = []

genIdentifier :: LexicalClass -> QC.Gen Name
genIdentifier lexicalClass = do
  spelling <- QC.elements $ case lexicalClass of
    VariableLike -> ["x", "map", "foldl'", "_worker", "value2"]
    ConstructorLike -> ["X", "Just", "Maybe", "Tree2", "Result'"]
  qualified <- QC.arbitrary
  if qualified
    then do moduleName <- genModule
            return (right (mkQualifiedIdentifier moduleName spelling))
    else return (right (mkIdentifier spelling))

genOperator :: LexicalClass -> QC.Gen Name
genOperator lexicalClass = do
  spelling <- QC.elements $ case lexicalClass of
    VariableLike -> ["+", "++", ".", "<*>", "$!", "--*"]
    ConstructorLike -> [":+", ":+:", ":*", ":<>"]
  qualified <- QC.arbitrary
  if qualified
    then do moduleName <- genModule
            return (right (mkQualifiedOperator moduleName spelling))
    else return (right (mkOperator spelling))

genTuple :: Boxity -> QC.Gen Name
genTuple Boxed = do
  arity <- QC.frequency [(1, return 0), (4, QC.chooseInt (2, 16))]
  return (right (tupleName Boxed arity))
genTuple Unboxed = do
  arity <- QC.frequency [(1, return 0), (4, QC.chooseInt (2, 16))]
  return (right (tupleName Unboxed arity))

genModule :: QC.Gen ModuleName
genModule = do
  count <- QC.chooseInt (1, 4)
  segments <- QC.vectorOf count $
    QC.elements ["M", "Data", "List", "Control", "X2", "Internal'"]
  return (right (mkModuleNameSegments segments))

right :: Show error => Either error value -> value
right (Right value) = value
right (Left problem) = error (show problem)

sealedEnvironment
  :: (Ord variable, Show variable)
  => [Declaration.Declaration variable Void annotation]
  -> Environment.Environment variable Void annotation
sealedEnvironment = right . Environment.mkEnvironment

validSourcePosition :: Int -> Int -> SourcePosition
validSourcePosition line column = right $ mkSourcePosition line column

validSourceSpan :: Int -> Int -> Int -> Int -> SourceSpan
validSourceSpan startLine startColumn endLine endColumn = right $ do
  start <- mkSourcePosition startLine startColumn
  end <- mkSourcePosition endLine endColumn
  mkSourceSpan start end

backticked :: String -> String
backticked source = [toEnum 96] ++ source ++ [toEnum 96]

reservedIdentifiers :: [String]
reservedIdentifiers =
  [ "_", "as", "case", "class", "data", "default", "deriving"
  , "do", "else", "foreign", "hiding", "if", "import", "in"
  , "infix", "infixl", "infixr", "instance", "let", "module"
  , "newtype", "of", "qualified", "then", "type", "where"
  ]

reservedOperators :: [String]
reservedOperators =
  [ "..", "--", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>" ]
