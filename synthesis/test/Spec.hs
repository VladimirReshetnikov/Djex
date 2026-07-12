module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Language.Haskell.Synthesis.Constraint
import Language.Haskell.Synthesis.Diagnostic
import qualified Language.Haskell.Synthesis.Declaration as Declaration
import qualified Language.Haskell.Synthesis.Environment as Environment
import Language.Haskell.Synthesis.Generated
import Language.Haskell.Synthesis.Name
import qualified Language.Haskell.Synthesis.Kind as Kind
import Language.Haskell.Synthesis.Search
import qualified Language.Haskell.Synthesis.Type as SharedType
import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "haskell-synthesis"
  [ constraintTests
  , declarationTests
  , environmentTests
  , generatedTests
  , searchTests
  , typeTests
  , moduleTests
  , ordinaryTests
  , specialTests
  , parserTests
  , diagnosticTests
  , localOption (QC.QuickCheckTests 1000) propertyTests
  ]

environmentTests :: TestTree
environmentTests = testGroup "environments"
  [ testCase "index declarations across shared namespaces" $ do
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
      Declaration.validateDeclaration
          (Declaration.TypeSynonymDeclaration () valueName [] variable)
        @?= Left (Declaration.InvalidDeclaredTypeName valueName)
      Declaration.validateDeclaration duplicateConstructors
        @?= Left (Declaration.DuplicateConstructorName typeName)
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
  ]

typeTests :: TestTree
typeTests = testGroup "source types"
  [ testCase "canonicalize saturated function and tuple constructors" $ do
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
  , testCase "reject malformed tuples, constructors, and quantifiers" $ do
      let variableName = right $ mkIdentifier "value"
      SharedType.validateType
          (SharedType.TupleType Boxed [SharedType.TypeVariable (0 :: Int)])
        @?= Left (SharedType.InvalidTupleTypeArity Boxed 1)
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

searchTests :: TestTree
searchTests = testGroup "search status"
  [ testCase "finished and truncated completions stay distinct" $ do
      Completed Finished @?= Completed Finished
      truncated StepLimitReached @?=
        Truncated (StepLimitReached :| [])
  , testCase "truncation retains every independent pruning reason" $ do
      let completion = Truncated
            (QueueLimitPruned 7 :| [DepthLimitPruned 2])
      _ <- evaluate $ force completion
      completion @?= Truncated
        (QueueLimitPruned 7 :| [DepthLimitPruned 2])
  , testCase "batch functor changes candidates only" $
      fmap (+ 1) (SearchBatch Continuing "stats" [1 :: Int, 2]) @?=
        SearchBatch Continuing "stats" [2, 3]
  ]

generatedTests :: TestTree
generatedTests = testGroup "generated syntax"
  [ testCase "validate lambda, let, and case scopes" $ do
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
  , testCase "render lambdas, tuples, and symbolic applications" $ do
      let plus = right $ mkOperator "+"
          true = right $ mkIdentifier "True"
          false = right $ mkIdentifier "False"
          expression = Lambda [Bind (0 :: Int)] $
            Apply (Apply (Global plus) (Local 0)) $
              Tuple [Global true, Global false]
      renderExpression (defaultRenderOptions (const "x")) expression @?=
        Right "\\x -> x + (True, False)"
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
          just = right $ mkIdentifier "Just"
          nothing = right $ mkIdentifier "Nothing"
          clause = FunctionClause select [Bind (0 :: Int)] $
            Case (Local 0)
              [ (Constructor just [Bind 1], Local 1)
              , (Constructor nothing [], Local 0)
              ]
          options = RenderOptions FullyQualified (const "select") []
      validateFunctionClauseScope clause @?= Right ()
      renderFunctionClause options clause @?=
        Right (unlinesWithoutFinal
          [ "select select' ="
          , "  case select' of"
          , "  Just select'' -> select''"
          , "  Nothing -> select'"
          ])
  , testCase "render holes and reject malformed surface shapes" $ do
      let options = defaultRenderOptions (\local -> 't' : show (local :: Int))
          variableName = right $ mkIdentifier "value"
      renderExpression options (Hole 3) @?= Right "_t3"
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
      assertTuple Unboxed 1 "(# #)"
      assertTuple Unboxed 2 "(#,#)"
      assertTuple Unboxed 3 "(#,,#)"
      assertTuple Unboxed 8 "(#,,,,,,,#)"
  , testCase "reject invalid boxed arities" $
      forM_ [-3, -1, 1] $ \arity -> do
        tupleName Boxed arity @?= Left (InvalidTupleArity Boxed arity)
        specialName (TupleConstructor Boxed arity) @?=
          Left (InvalidTupleArity Boxed arity)
  , testCase "reject invalid unboxed arities" $
      forM_ [-3, -1, 0] $ \arity -> do
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
      parseName "(# #)" @?= tupleName Unboxed 1
      parseName "(#,#)" @?= tupleName Unboxed 2
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
  , testCase "code, source, span, and ordered context" $ do
      let span' = SourceSpan (SourcePosition 3 7) (SourcePosition 3 12)
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
      let span' = SourceSpan (SourcePosition 4 2) (SourcePosition 6 9)
      renderDiagnostic (withSpan span' (diagnostic Warning "ambiguous name"))
        @?= "4:2-6:9: warning: ambiguous name"
  , testCase "point span and empty code" $ do
      let point = SourcePosition 1 1
          value = withCode "" $ withSpan (SourceSpan point point) $
            diagnostic Info "using default search budget"
      renderDiagnostic value @?= "1:1: info: using default search budget"
  , testCase "total NFData instances" $ do
      _ <- evaluate (force (right (parseName "Data.List.(++)")))
      _ <- evaluate (force (withContext "query" (diagnostic Error "failure")))
      return ()
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
  arity <- QC.chooseInt (1, 16)
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
