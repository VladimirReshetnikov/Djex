{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (SomeException, bracket, evaluate, try)
import Control.Monad (forM_)
import Data.Monoid (Any (..))
import Data.Bifunctor (first)
import Data.Either (rights)
import Data.Functor.Identity (Identity, runIdentity)
import Data.List (find, isInfixOf, isPrefixOf)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Numeric.Natural (Natural)
import System.Directory
  ( createDirectory
  , getTemporaryDirectory
  , removeFile
  , removePathForcibly
  )
import System.IO (hClose, hPutStr, openTempFile)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertEqual, testCase)
import Control.Monad.Trans.Except (catchE, runExceptT, throwE)
import qualified Language.Haskell.Exts.Syntax as HSE
import qualified Language.Haskell.Exts.Parser as HSE
import qualified Language.Haskell.Exts.Pretty as HSE
import qualified Language.Haskell.Exts.Extension as HSE
import qualified Language.Haskell.Exts.SrcLoc as HSE

import Language.Haskell.Exference.Core
  ( ExferenceChunkElement (..)
  , ExferenceBatchMetadata (..)
  , ExferenceCandidateDetails (..)
  , ExferenceEnvironment
  , ExferenceQuery (..)
  , ExferenceResult
  , ExferenceSourceTypeVariableHints
  , ExferenceSourceTypeVariableHintError (..)
  , ExferenceHeuristicsConfig (..)
  , SearchCompletion (..)
  , SearchStatus (..)
  , constraintsRelaxedAtStep
  , defaultHeuristicsConfig
  , emptyExferenceSourceTypeVariableHints
  , findExpressionsWithStatsEither
  , findQueryResultsInEnvironmentEither
  , mkExferenceEnvironment
  , mkExferenceSourceTypeVariableHints
  , validateExferenceQuery
  , validateExferenceInput
  )
import qualified Language.Haskell.Exference.Core as Core
import Language.Haskell.Exference.Core.ConstraintSolver
import Language.Haskell.Exference.Core.Declaration
import Language.Haskell.Exference.Core.Expression
  ( Expression (..)
  , ExpressionRenderError (..)
  , expressionTypedLocals
  , expressionNameHints
  , renderExpression
  , showExpression
  , toGeneratedExpression
  )
import Language.Haskell.Exference.Core.ExpressionCheck
  hiding (UnsupportedNestedForall)
import Language.Haskell.Exference.Core.ExpressionSimplify (simplifyExpression)
import Language.Haskell.Exference.Core.FunctionBinding
  ( ConstructorBinding (..)
  , DeconstructorBinding (..)
  , DeconstructorValidationError (..)
  , EnvironmentDuplicateError (..)
  , EnvironmentRatingError (..)
  , EnvironmentSyntaxError (..)
  , EnvDictionary (..)
  , FunctionBinding (..)
  , deconstructorBindingType
  , deconstructorBindingTypes
  , environmentBindingTypes
  , environmentConstraints
  , functionBindingFromType
  , functionBindingSignature
  , functionBindingType
  , functionBindingTypes
  , mapDeconstructorBindingTypes
  , mapFunctionBindingTypes
  , validateDeconstructorBinding
  )
import Language.Haskell.Exference.Core.RigidInstantiation
  ( RigidInstantiationError (..)
  , mkRigidInstantiationContext
  , planRigidInstantiation
  , rigidInstantiations
  )
import qualified Language.Haskell.Exference.Core.Score as Score
import qualified Language.Haskell.Exference.Core.Internal.Scope as Scope
import Language.Haskell.Exference.Core.TypeUtils hiding (largestId)
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.Unify
import Language.Haskell.Exference
  ( ExferenceInput (..)
  , ExferenceInputError (..)
  , ExferenceStats (..)
  , Penalty (..)
  , findExpressionsEither
  )
import DeprecatedCompatibility
  ( findExpressionsWithStats
  , findOneExpression
  , largestId
  )
import Language.Haskell.Exference.EnvironmentParser
  ( EnvironmentLoadError (..)
  , LoadReport (..)
  , SourceBinding (..)
  , SourceEnvironment (..)
  , UnsupportedVocabularyForm (..)
  , UnsupportedVocabularyOccurrence (..)
  , unsupportedVocabularyOccurrences
  , sourceBindingFunction
  , sourceFunctions
  , sourceTypeSynonymMap
  , checkedSourceInventory
  , checkedSourcePreparedInventory
  , checkedSourceProjection
  , checkSourceEnvironment
  , environmentLoadErrorDiagnostics
  , environmentFromFiles
  , environmentFromSources
  , environmentFromSourcesWithTypeVisibility
  , environmentFromModule
  , environmentFromModuleAndRatings
  , environmentFromPath
  , maximumBuiltInTupleArity
  , parseModules
  , parseModuleSources
  , parseRatings
  , toSynthesisSourceEnvironment
  , toSynthesisSourceInventory
  )
import Language.Haskell.Djex.Exference
  ( ExferenceOmission (..)
  , ExferenceOmissionReason (..)
  , ExferenceOptions (..)
  , ExferenceSessionPolicy (..)
  , defaultExferenceSessionPolicy
  , defaultExferenceOptions
  , exferenceRequestQuery
  , exferenceSessionDiagnostics
  , exferenceSessionOmissions
  , mkExferenceSessionWithPolicy
  )
import Language.Haskell.Djex.Exference.HaskellSrc
  ( ExferenceQueryScope (..)
  , ExferenceSessionLoadReport (..)
  , loadExferenceSession
  , loadExferenceSessionFromFiles
  , loadExferenceSessionFromFilesWithPolicy
  , loadExferenceSessionFromSources
  , loadExferenceSessionWithPolicy
  , parseExferenceRequest
  , parseExferenceRequestInScope
  )
import qualified Language.Haskell.Exference.Session as ExferenceSession
import Language.Haskell.Exference.ClassEnvFromHaskellSrc
  ( ClassEnvironmentLoadError (..)
  , ClassMethodDeclaration (..)
  , LoadedClassEnvironment (..)
  , loadClassEnvironment
  )
import Language.Haskell.Exference.Diagnostic
import Language.Haskell.Exference.ExpressionToHaskellSrc
  ( HaskellSrcConversionError (..)
  , expressionToHaskellSrc
  , functionToHaskellSrc
  , generatedExpressionToHaskellSrc
  , generatedFunctionClauseToHaskellSrc
  )
import Language.Haskell.Exference.BindingsFromHaskellSrc
  (getDataConss, getDataTypes, getDecls)
import Language.Haskell.Exference.ExtractionError
  ( ExtractionError (..)
  , extractionError
  )
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
  ( HsTypeDecl (..)
  , applyTypeDecls
  , fromSynthesisTypeDeclaration
  , getTypeDecls
  , getTypeDeclsLocated
  , parseType
  , parseTypeWithKinds
  , toSynthesisTypeDeclaration
  )
import Language.Haskell.Exference.TypeFromHaskellSrc
  ( ConversionT
  , TypeResolver
  , convDataFromTypeVarIndex
  , convDataReservedIds
  , convDataTypeVarIndex
  , convertTypeNoDecl
  , convertTypeNoDeclInternal
  , convertTypeNoDeclWithResolver
  , emptyConvData
  , getVar
  , haskellSrcExtsParseMode
  , legacyTypeResolver
  , parseQualifiedName
  , convertName
  , convertModuleName
  , convertQName
  , normalizeConvertedForalls
  , runConversionTWithState
  , scopeTypeResolverWithQualifiedNames
  )
import Language.Haskell.Exference.SimpleDict (emptyClassEnv)
import qualified Language.Haskell.Exference.SimpleDict as SimpleDict
  ( defaultHeuristicsConfig )
import qualified Language.Haskell.Synthesis.Constraint as SharedConstraint
import qualified Language.Haskell.Synthesis.Candidate as SharedCandidate
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Generated as Generated
import qualified Language.Haskell.Synthesis.Query as SharedQuery
import qualified Language.Haskell.Synthesis.Declaration as SharedDeclaration
import qualified Language.Haskell.Synthesis.Environment as SharedEnvironment
import qualified Language.Haskell.Synthesis.Kind as SharedKind
import qualified Language.Haskell.Synthesis.KindInference as SharedKindInference
import qualified Language.Haskell.Synthesis.Inventory as SharedInventory
import qualified Language.Haskell.Synthesis.Search as SharedSearch
import qualified Language.Haskell.Synthesis.Type as SharedType
import qualified Language.Haskell.Synthesis.TypeSynonym as SharedTypeSynonym
import qualified CompatibilityImport
import Paths_djex (getDataFileName)

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
      , testCase "known class arity rejects cyclic argument spines finitely" $ do
          let unary = HsTypeClass (name "Unary") [0] []
              argument = TypeCons $ name "Int"
              malformed = HsConstraint (name "Unary") $ repeat argument
              malformedInstance = HsInstance [] malformed
          environment <- expectRight $ mkStaticClassEnv [unary] []
          mkStaticClassEnv [unary] [malformedInstance] @?= Left
            (ConstraintArityMismatch InstanceHead (name "Unary") 1 2)
          validateKnownConstraintInEnv environment QueryConstraint malformed
            @?= Left (ConstraintArityMismatch
              QueryConstraint (name "Unary") 1 2)
          Set.size (inflateHsConstraints environment $ Set.singleton malformed)
            @?= 1
          length (inflateInstances environment [malformedInstance]) @?= 1
      , testCase "instance preflight bounds nested tuple spines" $ do
          let className = name "Unary"
              unary = HsTypeClass className [0] []
              integer = TypeCons $ name "Int"
              oversizedTuple = TypeTuple Boxed $ repeat integer
              malformedInstance = HsInstance []
                $ HsConstraint className [oversizedTuple]
          case mkStaticClassEnv [unary] [malformedInstance] of
            Left (InvalidConstraintArgument
                InstanceHead actualClass 0
                (InvalidSynthesisType
                  (SharedType.InvalidTupleTypeArity Boxed actualArity))) -> do
              actualClass @?= className
              actualArity @?= SharedName.maximumTupleArity + 1
            result -> fail $ "nested tuple preflight returned: " ++ show result
      , testCase "checked search boundaries preflight cyclic class arities" $ do
          let className = name "Unary"
              unary = HsTypeClass className [0] []
              integer = TypeCons $ name "Int"
              malformed = HsConstraint className $ repeat integer
              bindingName = name "malformed"
              binding = FunctionBinding integer bindingName 0 [malformed] []
          classes <- expectRight $ mkStaticClassEnv [unary] []
          case mkExferenceEnvironment (EnvDictionary [binding] [] classes) of
            Left failure -> failure @?= InvalidClassConstraint
              (ConstraintArityMismatch
                (BindingConstraint bindingName) className 1 2)
            Right _ -> fail "a cyclic binding constraint was accepted"
          validateExferenceInput identityInput
              { input_goalType = integer
              , input_envFuncs = [binding]
              , input_envClasses = classes
              }
            @?= Left (InvalidClassConstraint $ ConstraintArityMismatch
              (BindingConstraint bindingName) className 1 2)
          sealed <- expectRight
            $ mkExferenceEnvironment $ EnvDictionary [] [] classes
          let query = ExferenceQuery
                { queryGoalType = TypeForall [] [malformed] integer
                , queryExcludedBindings = Set.empty
                , querySearchOptions = defaultExferenceOptions
                }
          case validateExferenceQuery sealed query of
            Left failure -> failure @?= InvalidClassConstraint
              (ConstraintArityMismatch QueryConstraint className 1 2)
            Right _ -> fail "a cyclic query constraint was accepted"
          case validateExferenceQuery sealed query
              { querySearchOptions = defaultExferenceOptions
                  { exferenceMaximumSteps = 0 }
              } of
            Left failure -> failure @?= InvalidMaxSteps 0
            Right _ -> fail "an invalid step limit was accepted"
      , testCase "checked search boundaries bound tuple-width preflights" $ do
          classes <- expectRight $ mkStaticClassEnv [] []
          let integer = TypeCons $ name "Int"
              oversized = TypeTuple Boxed $ repeat integer
              options = defaultExferenceOptions
              query = ExferenceQuery oversized Set.empty options
              expectTupleWidth result = case result of
                Left (InvalidInputType _ (InvalidSynthesisType
                    (SharedType.InvalidTupleTypeArity Boxed actual))) ->
                  actual @?= SharedName.maximumTupleArity + 1
                _ -> fail "tuple-width preflight returned an unexpected result"
          sealed <- expectRight
            $ mkExferenceEnvironment $ EnvDictionary [] [] classes
          expectTupleWidth $ validateExferenceQuery sealed query
          expectTupleWidth $ mkExferenceEnvironment $ EnvDictionary
            [FunctionBinding oversized (name "wide") 0 [] []] [] classes
          expectTupleWidth $ mkExferenceEnvironment $ EnvDictionary []
            [DeconstructorBinding oversized [] False] classes
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
                [ TypeApp (TypeCons $ name "Maybe") (TypeVar 0)
                , TypeConstant 1
                ]
          shared <- expectRight $ toSynthesisConstraint constraint
          shared @?= SharedConstraint.Constraint
            (name "C")
            [ SharedType.TypeApplication
                (SharedType.TypeConstructor
                  $ name "Maybe")
                (SharedType.TypeVariable
                  $ SharedType.FlexibleVariable 0)
            , SharedType.TypeVariable $ SharedType.RigidVariable 1
            ]
          fromSynthesisConstraint shared @?= Right constraint
      , testCase "shared constraint conversion validates class identity" $ do
          let invalid = HsConstraint (name "notAClass") [TypeVar 0]
              sharedName = name "notAClass"
          toSynthesisConstraint invalid @?= Left
            (InvalidSynthesisConstraint
              $ SharedConstraint.InvalidConstraintClass sharedName)
      , testCase "native constraint aliases share validation precedence" $ do
          let invalidClass = name "notAClass"
              malformed = HsConstraint invalidClass
                [TypeTuple Boxed [TypeVar 0]]
              expected = Left $ InvalidSynthesisConstraint
                $ SharedConstraint.InvalidConstraintClass invalidClass
          -- Both native compatibility names describe the same boundary.  A
          -- malformed argument must not make their diagnostics disagree about
          -- the independently invalid class name.
          toSynthesisConstraint malformed @?= expected
          fromSynthesisConstraint malformed @?= expected
      , testCase "shared constraints reject unboxed class names" $ do
          unboxed <- expectRight $ SharedName.tupleName SharedName.Unboxed 2
          let expected = InvalidSynthesisConstraint
                $ SharedConstraint.InvalidConstraintClass unboxed
          toSynthesisConstraint
            (HsConstraint unboxed [TypeVar 0])
            @?= Left expected
          case fromSynthesisConstraint
              (SharedConstraint.Constraint unboxed
                [SharedType.TypeVariable $ SharedType.FlexibleVariable 0]) of
            Left (InvalidSynthesisConstraint
                (SharedConstraint.InvalidConstraintClass actual)) ->
              actual @?= unboxed
            result -> fail $ "unboxed constraint was accepted: " ++ show result
      , testCase "class environments reject invalid native argument types" $ do
          let base = HsTypeClass (name "Base") [0] []
              derived = HsTypeClass (name "Derived") [0]
                [HsConstraint (name "Base")
                  [TypeTuple Boxed [TypeVar 0]]]
              invalidInstance = HsInstance []
                $ HsConstraint (name "Base")
                [TypeForallNative
                  [SharedType.RigidVariable 7] [] (TypeVar 0)]
          mkStaticClassEnv [base, derived] [] @?= Left
            (InvalidConstraintArgument
              (ClassSuperclass $ name "Derived")
              (name "Base")
              0
              (InvalidSynthesisType
                $ SharedType.InvalidTupleTypeArity Boxed 1))
          mkStaticClassEnv [base] [invalidInstance] @?= Left
            (InvalidConstraintArgument
              InstanceHead
              (name "Base")
              0
              (RigidForallBinder 7))
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
      , testCase "constraint solving canonicalizes both comparison sides" $ do
          let className = name "C"
              cls = HsTypeClass className [0] []
              integer = TypeCons $ name "Int"
              boolean = TypeCons $ name "Bool"
              structural = HsConstraint className
                [TypeArrow integer boolean]
              application = HsConstraint className
                [TypeApp (TypeApp (TypeCons SharedName.functionName) integer)
                  boolean]
          staticEnvironment <- expectRight $ mkStaticClassEnv [cls] []
          filterUnresolved
              (mkQueryClassEnv staticEnvironment [structural]) [application]
            @?= Just []
          filterUnresolved
              (mkQueryClassEnv staticEnvironment []) [application]
            @?= Just [structural]
      , testCase "low-level class inflation preserves native rigid binders" $ do
          let base = HsTypeClass (name "Base") [0] []
              derived = HsTypeClass (name "Derived") [0]
                [HsConstraint (name "Base") [TypeVar 0]]
              rigidArgument = TypeForallNative
                [SharedType.RigidVariable 7] [] (TypeVar 1)
              sourceConstraint = HsConstraint (name "Derived")
                [rigidArgument]
              impliedConstraint = HsConstraint (name "Base")
                [rigidArgument]
              sourceInstance = HsInstance [] sourceConstraint
              completedInstance = HsInstance [impliedConstraint] sourceConstraint
          environment <- expectRight
            $ mkStaticClassEnv [base, derived] []
          let queryEnvironment = mkQueryClassEnv environment
                [sourceConstraint]
          qClassEnv_inflatedConstraints queryEnvironment @?=
            Set.fromList [sourceConstraint, impliedConstraint]
          inflateInstances environment [sourceInstance] @?=
            [completedInstance]
      , testCase "constraint and instance closure share parameter substitution" $ do
          let base = HsTypeClass (name "Base") [0, 1] []
              derived = HsTypeClass (name "Derived") [2, 3]
                [HsConstraint (name "Base") [TypeVar 3, TypeVar 2]]
              integer = TypeCons $ name "Int"
              boolean = TypeCons $ name "Bool"
              sourceConstraint = HsConstraint (name "Derived")
                [integer, boolean]
              impliedConstraint = HsConstraint (name "Base")
                [boolean, integer]
              prerequisite = HsConstraint (name "Base")
                [integer, boolean]
              sourceInstance = HsInstance [prerequisite] sourceConstraint
              completedInstance = HsInstance
                [prerequisite, impliedConstraint] sourceConstraint
          environment <- expectRight
            $ mkStaticClassEnv [base, derived] []
          inflateHsConstraints environment (Set.singleton sourceConstraint)
            @?= Set.fromList [sourceConstraint, impliedConstraint]
          inflateInstances environment [sourceInstance] @?=
            [completedInstance]
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
      , testCase "alpha-renamed instance heads report first repetitions" $ do
          let classC = HsTypeClass (name "C") [0] []
              classD = HsTypeClass (name "D") [0] []
              firstC = HsConstraint (name "C") [TypeVar 1]
              firstD = HsConstraint (name "D") [TypeVar 2]
              repeatedC = HsConstraint (name "C") [TypeVar 8]
              repeatedD = HsConstraint (name "D") [TypeVar 9]
              repeatedCAgain = HsConstraint (name "C") [TypeVar 10]
              instanceOf = HsInstance []
          mkStaticClassEnv [classC, classD]
              (map instanceOf
                [firstC, firstD, repeatedC, repeatedD, repeatedCAgain]) @?=
            Left (DuplicateInstanceHeads [repeatedC, repeatedD])
      , testCase "pairwise-overlapping explicit instance heads are rejected" $ do
          let cls = HsTypeClass (name "C") [0, 1] []
              integer = TypeCons $ name "Int"
              boolean = TypeCons $ name "Bool"
              genericHead = HsConstraint (name "C")
                [TypeVar 0, TypeVar 0]
              incompatibleHead = HsConstraint (name "C")
                [integer, boolean]
              compatibleHead = HsConstraint (name "C")
                [integer, integer]
              instanceOf = HsInstance []
          mkStaticClassEnv [cls]
              (map instanceOf
                [genericHead, incompatibleHead, compatibleHead]) @?=
            Left (OverlappingInstanceHeads [(genericHead, compatibleHead)])
      , testCase "duplicate instance identity respects nested forall scopes" $ do
          let cls = HsTypeClass (name "C") [0] []
              firstHead = HsConstraint (name "C")
                [ TypeForall [0] []
                    $ TypeArrow (TypeVar 1) (TypeVar 0)
                ]
              renamedHead = HsConstraint (name "C")
                [ TypeForall [7] []
                    $ TypeArrow (TypeVar 8) (TypeVar 7)
                ]
          mkStaticClassEnv [cls]
              [HsInstance [] firstHead, HsInstance [] renamedHead] @?=
            Left (DuplicateInstanceHeads [renamedHead])
      , testCase "class declarations require the constructor namespace" $ do
          let lowercase = HsTypeClass (name "className") [] []
              tupleName = validTupleName 2
              tuple = HsTypeClass tupleName [] []
          mkStaticClassEnv [lowercase] []
            @?= Left (InvalidClassName $ name "className")
          mkStaticClassEnv [tuple] []
            @?= Left (InvalidClassName tupleName)
      , testCase "duplicate class parameters are rejected" $ do
          let malformed = HsTypeClass (name "C") [0, 0] []
          mkStaticClassEnv [malformed] []
            @?= Left (DuplicateClassParameter (name "C") 0)
      , testCase "class validation follows declaration order" $ do
          let firstName = name "First"
              firstMalformed = HsTypeClass firstName [0, 0] []
              laterName = name "Later"
              laterUnknownSuperclass = HsTypeClass laterName [0]
                [HsConstraint (name "Missing") [TypeVar 0]]
              laterValid = HsTypeClass laterName [0] []
              laterDuplicate = HsTypeClass laterName [1] []
              expected = Left $ DuplicateClassParameter firstName 0
          -- Neither a global superclass preflight nor a whole-table duplicate
          -- scan may select a failure after the first source declaration.
          mkStaticClassEnv
              [firstMalformed, laterUnknownSuperclass] [] @?= expected
          mkStaticClassEnv
              [firstMalformed, laterValid, laterDuplicate] [] @?= expected
      , testCase "duplicate classes fail at their source occurrence" $ do
          let repeatedName = name "Repeated"
              repeated = HsTypeClass repeatedName [0] []
              interveningName = name "Intervening"
              intervening = HsTypeClass interveningName [0, 0] []
          mkStaticClassEnv [repeated, intervening, repeated] [] @?=
            Left (DuplicateClassParameter interveningName 0)
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
              expected = "duplicate type class: C (M.C)"
              -- The duplicate is attributed to the first declaration of the
              -- name in source order: line 2 in either declaration order.
              check label result = case result of
                Left (ClassDeclarationErrors (failure :| [])) -> do
                  assertEqual (label ++ " message") expected
                    $ extractionErrorMessage failure
                  case extractionErrorLocation failure of
                    Nothing -> fail $ label ++ " lost its source location"
                    Just location -> do
                      locationSource location @?= "qualified-class-test.hs"
                      sourceLine (sourceStart $ locationSpan location) @?= 2
                other -> fail $ label ++ ": unexpected result: " ++ show other
          forwardResult <- classEnvironmentFromSources
            [source unary binary]
          reverseResult <- classEnvironmentFromSources
            [source binary unary]
          check "forward duplicate" forwardResult
          check "reverse duplicate" reverseResult
      , testCase "frontend rejects unannotated overlapping instances" $ do
          result <- classEnvironmentFromSources
            [ unlines
                [ "module M where"
                , "class C a where"
                , "instance C a"
                , "instance C Int"
                ]
            ]
          case result of
            Left (InvalidClassEnvironment
                (OverlappingInstanceHeads [(_, _)])) -> pure ()
            other -> fail $ "overlapping instances were accepted: "
              ++ show other
      , testCase "superclass arity is checked against the class table" $ do
          let binary = HsTypeClass (name "Binary") [0, 1] []
              derived = HsTypeClass (name "Derived") [0]
                [HsConstraint (name "Binary") [TypeVar 0]]
          mkStaticClassEnv [binary, derived] []
            @?= Left (ConstraintArityMismatch
              (ClassSuperclass $ name "Derived") (name "Binary") 2 1)
      , testCase "frontend diagnoses too few and too many superclass arguments" $ do
          let expected =
                [ ( "wrong number of parameters for type class C: "
                      ++ "expected 2, got 3"
                  , 3
                  )
                , ( "wrong number of parameters for type class C: "
                      ++ "expected 2, got 1"
                  , 4
                  )
                ]
          result <- classEnvironmentFromSources
            [ unlines
                [ "module M where"
                , "class C a b where"
                -- Reverse nominal order: a Map-ordered elaboration would
                -- incorrectly report A before Z.
                , "class C a b a => Z a where"
                , "class C a => A a where"
                ]
            ]
          case result of
            Left (ClassDeclarationErrors (firstError :| remaining)) -> do
              let failures = firstError : remaining
                  summarize failure = do
                    location <- extractionErrorLocation failure
                    pure
                      ( extractionErrorMessage failure
                      , locationSource location
                      , sourceLine $ sourceStart $ locationSpan location
                      )
              map summarize failures @?=
                map (\(message, line) -> Just
                  (message, "qualified-class-test.hs", line)) expected
            other -> fail $ "malformed superclasses were accepted: " ++ show other
      , testCase "frontend preserves source order for semantic class errors" $ do
          result <- classEnvironmentFromSources
            [ unlines
                [ "module M where"
                -- Reverse lexical order: passing Map.elems to the core would
                -- incorrectly select A's duplicate parameter before Z's.
                , "class Z a a where"
                , "class A b b where"
                ]
            ]
          zName <- expectRight $ mkQualifiedName ["M"] "Z"
          result @?= Left (InvalidClassEnvironment
            $ DuplicateClassParameter zName 0)
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
              let errors = map extractionErrorMessage
                    $ firstError : remaining
              in assertBool
                  ("missing explicit-forall diagnostic: " ++ show errors)
                  $ any ("outside its explicit forall" `isInfixOf`) errors
            other -> fail $ "malformed instance was accepted: " ++ show other
      , testCase "duplicate instance binders precede head validation" $ do
          result <- classEnvironmentFromSources
            [ unlines
                [ "module M where"
                , "instance forall a a. Missing a"
                ]
            ]
          case result of
            Left (InstanceDeclarationErrors (failure :| [])) -> do
              extractionErrorMessage failure @?=
                "duplicate explicitly quantified instance variable"
              -- The failing instance declaration sits on line 2.
              case extractionErrorLocation failure of
                Nothing -> fail "instance rejection lost its source location"
                Just location ->
                  sourceLine (sourceStart $ locationSpan location) @?= 2
            other -> fail $ "unexpected duplicate-binder result: "
              ++ show other
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
      , testCase "instance completion adds nominal superclass prerequisites" $ do
          let base = HsTypeClass (name "Base") [0] []
              derived = HsTypeClass (name "Derived") [7]
                [HsConstraint (name "Base") [TypeVar 7]]
              integer = TypeCons $ name "Int"
              sourceInstance = HsInstance []
                $ HsConstraint (name "Derived") [integer]
              superclassConstraint = HsConstraint (name "Base") [integer]
              completedInstance = HsInstance [superclassConstraint]
                $ instance_head sourceInstance
          environment <- expectRight
            $ mkStaticClassEnv [base, derived] [sourceInstance]
          sClassEnv_explicitInstances environment @?= [sourceInstance]
          Map.lookup (name "Base") (sClassEnv_instances environment) @?= Nothing
          Map.lookup (name "Derived") (sClassEnv_instances environment)
            @?= Just [completedInstance]
      , testCase "class methods attach to the exactly qualified class" $ do
          classAName <- expectRight $ mkQualifiedName ["A"] "C"
          classBName <- expectRight $ mkQualifiedName ["B"] "C"
          moduleA <- expectParsedModule $ unlines
            [ "module A where"
            , "class C a where"
            , "  method :: a -> a"
            ]
          moduleB <- expectParsedModule $ unlines
            [ "module B where"
            , "class C a"
            ]
          loaded <- expectRight $ runIdentity
            $ loadClassEnvironment [] Map.empty [moduleA, moduleB]
          let methods = concat $ loadedClassMethodsByModule loaded
          Map.keys (sClassEnv_tclasses $ loadedStaticClassEnvironment loaded)
            @?= [classAName, classBName]
          case rights methods of
            [ClassMethodDeclaration owner binding] -> do
              owner @?= classAName
              case functionConstraints binding of
                constraint : _ ->
                  constraint_tclass constraint @?= classAName
                [] -> fail "class method omitted its owner constraint"
            result -> fail $ "unexpected class methods: " ++ show result
      , testCase "class methods preserve lexical forall identities" $ do
          parsedModule <- expectParsedModule $ unlines
            [ "{-# LANGUAGE RankNTypes #-}"
            , "module LexicalMethods where"
            , "class Outer a"
            , "class Inner a"
            , "class Depends a"
            , "class Owner a where"
            , "  shadowed :: forall a. Outer a => forall a. Inner a => a -> a"
            , "  ownerReferenced :: forall b. Depends a => b -> b"
            , "  ordinary :: a -> a"
            ]
          ownerName <- expectRight
            $ mkQualifiedName ["LexicalMethods"] "Owner"
          outerName <- expectRight
            $ mkQualifiedName ["LexicalMethods"] "Outer"
          innerName <- expectRight
            $ mkQualifiedName ["LexicalMethods"] "Inner"
          dependsName <- expectRight
            $ mkQualifiedName ["LexicalMethods"] "Depends"
          loaded <- expectRight $ runIdentity
            $ loadClassEnvironment [] Map.empty [parsedModule]
          case rights $ concat $ loadedClassMethodsByModule loaded of
            [ ClassMethodDeclaration shadowedOwner shadowed
              , ClassMethodDeclaration referencedOwner ownerReferenced
              , ClassMethodDeclaration ordinaryOwner ordinary
              ] -> do
                shadowedOwner @?= ownerName
                referencedOwner @?= ownerName
                ordinaryOwner @?= ownerName
                functionConstraints shadowed @?=
                  [ HsConstraint ownerName [TypeVar 0]
                  , HsConstraint outerName [TypeVar 1]
                  , HsConstraint innerName [TypeVar 2]
                  ]
                functionParameters shadowed @?= [TypeVar 2]
                functionResult shadowed @?= TypeVar 2
                functionConstraints ownerReferenced @?=
                  [ HsConstraint ownerName [TypeVar 0]
                  , HsConstraint dependsName [TypeVar 0]
                  ]
                functionParameters ownerReferenced @?= [TypeVar 3]
                functionResult ownerReferenced @?= TypeVar 3
                functionConstraints ordinary @?=
                  [HsConstraint ownerName [TypeVar 0]]
                functionParameters ordinary @?= [TypeVar 0]
                functionResult ordinary @?= TypeVar 0
            result -> fail $ "unexpected lexical class methods: " ++ show result
      , testCase "exact variable-bearing givens discharge before deferral" $ do
          let cls = HsTypeClass (name "C") [0] []
              given = HsConstraint (name "C") [TypeVar 0]
          staticEnvironment <- expectRight $ mkStaticClassEnv [cls] []
          let environment = mkQueryClassEnv staticEnvironment [given]
          isPossible environment [given] @?= Just []
          filterUnresolved environment [given] @?= Just []
      , testCase "cyclic instance prerequisites are refuted during search" $ do
          let cls = HsTypeClass (name "C") [0] []
              prerequisite = HsConstraint (name "C") [TypeVar 0]
              query = HsConstraint (name "C") [TypeCons $ name "Int"]
              cyclicInstance = HsInstance [prerequisite] prerequisite
          staticEnvironment <- expectRight
            $ mkStaticClassEnv [cls] [cyclicInstance]
          let environment = mkQueryClassEnv staticEnvironment []
          isPossible environment [query] @?= Nothing
          filterUnresolved environment [query] @?= Just [query]
      , testCase "expanding instance prerequisites are rejected" $ do
          let className = name "C"
              cls = HsTypeClass className [0] []
              headConstraint = HsConstraint className [TypeVar 0]
              prerequisite = HsConstraint className
                [TypeApp (TypeCons SharedName.listName) (TypeVar 0)]
              growingInstance = HsInstance [prerequisite] headConstraint
          mkStaticClassEnv [cls] [growingInstance] @?= Left
            (ExpandingInstancePrerequisite headConstraint prerequisite)
      , testCase "shrinking instance prerequisites resolve normally" $ do
          let className = name "C"
              cls = HsTypeClass className [0] []
              integer = TypeCons $ name "Int"
              variableConstraint = HsConstraint className [TypeVar 0]
              listVariableConstraint = HsConstraint className
                [TypeApp (TypeCons SharedName.listName) (TypeVar 0)]
              integerConstraint = HsConstraint className [integer]
              listIntegerConstraint = HsConstraint className
                [TypeApp (TypeCons SharedName.listName) integer]
              baseInstance = HsInstance [] integerConstraint
              shrinkingInstance = HsInstance
                [variableConstraint] listVariableConstraint
          staticEnvironment <- expectRight $ mkStaticClassEnv [cls]
            [baseInstance, shrinkingInstance]
          let environment = mkQueryClassEnv staticEnvironment []
          isPossible environment [listIntegerConstraint] @?= Just []
          filterUnresolved environment [listIntegerConstraint] @?= Just []
      , testCase "termination includes completed superclass prerequisites" $ do
          let baseName = name "Base"
              derivedName = name "Derived"
              base = HsTypeClass baseName [0] []
              superclassConstraint = HsConstraint baseName
                [TypeApp (TypeCons SharedName.listName) (TypeVar 0)]
              derived = HsTypeClass derivedName [0]
                [superclassConstraint]
              sourceHead = HsConstraint derivedName [TypeVar 0]
              sourceInstance = HsInstance [] sourceHead
          mkStaticClassEnv [base, derived] [sourceInstance] @?= Left
            (ExpandingInstancePrerequisite
              sourceHead superclassConstraint)
      , testCase "subclass instances require rather than imply superclasses" $ do
          let baseName = name "A"
              derivedName = name "B"
              integer = TypeCons $ name "Int"
              base = HsTypeClass baseName [0] []
              derived = HsTypeClass derivedName [0]
                [HsConstraint baseName [TypeVar 0]]
              baseConstraint = HsConstraint baseName [integer]
              derivedConstraint = HsConstraint derivedName [integer]
              sourceInstance = HsInstance [] derivedConstraint
              completedInstance = HsInstance
                [baseConstraint] derivedConstraint
          staticEnvironment <- expectRight
            $ mkStaticClassEnv [base, derived] [sourceInstance]
          Map.lookup baseName (sClassEnv_instances staticEnvironment) @?= Nothing
          Map.lookup derivedName (sClassEnv_instances staticEnvironment)
            @?= Just [completedInstance]
          let environment = mkQueryClassEnv staticEnvironment []
          isPossible environment [baseConstraint] @?= Nothing
          isPossible environment [derivedConstraint] @?= Nothing
          filterUnresolved environment [baseConstraint]
            @?= Just [baseConstraint]
          filterUnresolved environment [derivedConstraint]
            @?= Just [baseConstraint]
          -- A supplied subclass dictionary contains its superclass
          -- dictionary, so query givens continue to close upward.
          filterUnresolved
              (mkQueryClassEnv staticEnvironment [derivedConstraint])
              [baseConstraint]
            @?= Just []
      , testCase "explicit superclass and subclass instances resolve cleanly" $ do
          let baseName = name "A"
              derivedName = name "B"
              integer = TypeCons $ name "Int"
              base = HsTypeClass baseName [0] []
              derived = HsTypeClass derivedName [0]
                [HsConstraint baseName [TypeVar 0]]
              baseConstraint = HsConstraint baseName [integer]
              derivedConstraint = HsConstraint derivedName [integer]
              baseInstance = HsInstance [] baseConstraint
              derivedInstance = HsInstance [] derivedConstraint
              completedDerived = HsInstance
                [baseConstraint] derivedConstraint
          staticEnvironment <- expectRight $ mkStaticClassEnv [base, derived]
            [baseInstance, derivedInstance]
          Map.lookup baseName (sClassEnv_instances staticEnvironment)
            @?= Just [baseInstance]
          Map.lookup derivedName (sClassEnv_instances staticEnvironment)
            @?= Just [completedDerived]
          let environment = mkQueryClassEnv staticEnvironment []
          isPossible environment [baseConstraint, derivedConstraint]
            @?= Just []
          filterUnresolved environment [baseConstraint, derivedConstraint]
            @?= Just []
      , testCase "superclass completion deduplicates prerequisites stably" $ do
          let baseName = name "A"
              otherName = name "Other"
              derivedName = name "B"
              integer = TypeCons $ name "Int"
              base = HsTypeClass baseName [0] []
              other = HsTypeClass otherName [0] []
              derived = HsTypeClass derivedName [0]
                [HsConstraint baseName [TypeVar 0]]
              baseConstraint = HsConstraint baseName [integer]
              otherConstraint = HsConstraint otherName [integer]
              derivedConstraint = HsConstraint derivedName [integer]
              sourceInstance = HsInstance
                [baseConstraint, otherConstraint] derivedConstraint
          staticEnvironment <- expectRight
            $ mkStaticClassEnv [base, other, derived] []
          inflateInstances staticEnvironment [sourceInstance] @?=
            [sourceInstance]
      ]
  , testGroup "Haskell source bindings"
      [ testCase "headerless signatures belong to the implicit Main module" $ do
          parsedModule <- expectParsedModule "identity :: a -> a"
          identityName <- expectRight $ mkQualifiedName ["Main"] "identity"
          let extracted = runIdentity
                $ getDecls [] Map.empty Map.empty [parsedModule]
          map (fmap functionName) extracted @?= [Right identityName]
      , testCase "headerless datatype, class, and method declarations survive" $ do
          parsedModule <- expectParsedModule $ unlines
            [ "data Box a = Box a"
            , "class C a where"
            , "  method :: a -> a"
            ]
          boxName <- expectRight $ mkQualifiedName ["Main"] "Box"
          className <- expectRight $ mkQualifiedName ["Main"] "C"
          methodName <- expectRight $ mkQualifiedName ["Main"] "method"
          dataTypes <- expectRight $ getDataTypes [parsedModule]
          dataTypes @?= [boxName]
          loaded <- expectRight $ runIdentity
            $ loadClassEnvironment dataTypes Map.empty [parsedModule]
          let classEnvironment = loadedStaticClassEnvironment loaded
              methods = concat $ loadedClassMethodsByModule loaded
              sourceInstanceCount :: Natural
              sourceInstanceCount = loadedSourceInstanceCount loaded
          sourceInstanceCount @?= 0
          Map.member className (sClassEnv_tclasses classEnvironment) @?= True
          map (fmap $ functionName . classMethodFunction) methods
            @?= [Right methodName]
          let deconstructors = runIdentity
                $ getDataConss (sClassEnv_tclasses classEnvironment)
                    dataTypes Map.empty [parsedModule]
          case deconstructors of
            [Right (_, binding)] ->
              typeConstructorHead (deconstructorInput binding) @?= Just boxName
            result -> fail $ "unexpected headerless datatype bindings: "
              ++ show result
      , testCase "explicit module headers retain their declared name" $ do
          parsedModule <- expectParsedModule $ unlines
            [ "module Explicit where"
            , "identity :: a -> a"
            ]
          identityName <- expectRight $ mkQualifiedName ["Explicit"] "identity"
          let extracted = runIdentity
                $ getDecls [] Map.empty Map.empty [parsedModule]
          map (fmap functionName) extracted @?= [Right identityName]
      , testCase "source bindings preserve rank-N lexical identity" $ do
          parsedModule <- expectParsedModule $ unlines
            [ "{-# LANGUAGE RankNTypes #-}"
            , "module RankNBinding where"
            , "ordinary :: (forall foo. foo -> foo) -> foo"
            ]
          ordinary <- expectRight
            $ mkQualifiedName ["RankNBinding"] "ordinary"
          let nested = TypeForall [1] []
                $ TypeArrow (TypeVar 1) (TypeVar 1)
              expected = FunctionBinding
                (TypeVar 0) ordinary 0 [] [nested]
              extracted = runIdentity
                $ getDecls [] Map.empty Map.empty [parsedModule]
          extracted @?= [Right expected]
      , testCase "monomorphic deconstructors have no empty forall wrapper" $ do
          parsedModule <- expectParsedModule $ unlines
            [ "module Fixture where"
            , "data Flag = Off | On"
            ]
          flag <- expectRight $ mkQualifiedName ["Fixture"] "Flag"
          let extracted = runIdentity
                $ getDataConss Map.empty [] Map.empty [parsedModule]
          case extracted of
            [Right (_, DeconstructorBinding input _ _)] ->
              input @?= TypeCons flag
            result -> fail $ "unexpected datatype bindings: " ++ show result
      , testCase "rank-N fields shadow and restore datatype parameters" $ do
          parsedModule <- expectParsedModule $ unlines
            [ "{-# LANGUAGE RankNTypes #-}"
            , "module RankNField where"
            , "data Box a = Box (forall a. a -> a) a"
            ]
          box <- expectRight $ mkQualifiedName ["RankNField"] "Box"
          makeBox <- expectRight $ mkQualifiedName ["RankNField"] "Box"
          let nested = TypeForall [1] []
                $ TypeArrow (TypeVar 1) (TypeVar 1)
              input = TypeApp (TypeCons box) (TypeVar 0)
              expectedFunction = FunctionBinding
                input makeBox 0 [] [nested, TypeVar 0]
              expectedDeconstructor = DeconstructorBinding input
                [ConstructorBinding makeBox [nested, TypeVar 0]] False
              extracted = runIdentity
                $ getDataConss Map.empty [] Map.empty [parsedModule]
          extracted @?= [Right ([expectedFunction], expectedDeconstructor)]
      , testCase "constructor forall scopes over fields and polymorphic result" $ do
          parsedModule <- expectParsedModule $ unlines
            [ "module Fixture where"
            , "data Choice a = This a | That"
            ]
          choice <- expectRight $ mkQualifiedName ["Fixture"] "Choice"
          this <- expectRight $ mkQualifiedName ["Fixture"] "This"
          that <- expectRight $ mkQualifiedName ["Fixture"] "That"
          let resultType = TypeApp (TypeCons choice) (TypeVar 0)
              extracted = runIdentity
                $ getDataConss Map.empty [] Map.empty [parsedModule]
          case extracted of
            [Right (constructors, DeconstructorBinding input fields False)] -> do
              input @?= resultType
              constructors @?=
                [ functionBindingFromType this 0
                    $ TypeForall [0] []
                    $ TypeArrow (TypeVar 0) resultType
                , functionBindingFromType that 0
                    $ TypeForall [0] [] resultType
                ]
              fields @?=
                [ ConstructorBinding this [TypeVar 0]
                , ConstructorBinding that []
                ]
            result -> fail $ "unexpected datatype bindings: " ++ show result
      , testCase "record constructors flatten fields and emit selectors once" $ do
          parsedModule <- expectParsedModule $ unlines
            [ "module Fixture where"
            , "data Record a"
            , "  = First { common :: a, pairLeft, pairRight :: (a, a) }"
            , "  | Second { common :: a }"
            ]
          dataTypes <- expectRight $ getDataTypes [parsedModule]
          record <- expectRight $ mkQualifiedName ["Fixture"] "Record"
          firstConstructor <- expectRight
            $ mkQualifiedName ["Fixture"] "First"
          secondConstructor <- expectRight
            $ mkQualifiedName ["Fixture"] "Second"
          common <- expectRight $ mkQualifiedName ["Fixture"] "common"
          pairLeft <- expectRight $ mkQualifiedName ["Fixture"] "pairLeft"
          pairRight <- expectRight $ mkQualifiedName ["Fixture"] "pairRight"
          let parameter = TypeVar 0
              recordType = TypeApp (TypeCons record) parameter
              pairType = TypeTuple Boxed [parameter, parameter]
              extracted = runIdentity
                $ getDataConss Map.empty dataTypes Map.empty [parsedModule]
          case extracted of
            [Right (bindings, DeconstructorBinding input constructors False)] -> do
              input @?= recordType
              map functionName bindings @?=
                [ firstConstructor
                , secondConstructor
                , common
                , pairLeft
                , pairRight
                ]
              length (filter ((== common) . functionName) bindings) @?= 1
              bindings @?=
                [ functionBindingFromType firstConstructor 0
                    $ TypeForall [0] []
                    $ TypeArrow parameter
                    $ TypeArrow pairType
                    $ TypeArrow pairType recordType
                , functionBindingFromType secondConstructor 0
                    $ TypeForall [0] []
                    $ TypeArrow parameter recordType
                , functionBindingFromType common 0
                    $ TypeForall [0] []
                    $ TypeArrow recordType parameter
                , functionBindingFromType pairLeft 0
                    $ TypeForall [0] []
                    $ TypeArrow recordType pairType
                , functionBindingFromType pairRight 0
                    $ TypeForall [0] []
                    $ TypeArrow recordType pairType
                ]
              constructors @?=
                [ ConstructorBinding firstConstructor
                    [parameter, pairType, pairType]
                , ConstructorBinding secondConstructor [parameter]
                ]
            result -> fail $ "unexpected record bindings: " ++ show result
      , testCase "infix constructors lower as binary constructors" $ do
          parsedModule <- expectParsedModule $ unlines
            [ "module Fixture where"
            , "data Chain a = a :>: a"
            ]
          dataTypes <- expectRight $ getDataTypes [parsedModule]
          chain <- expectRight $ mkQualifiedName ["Fixture"] "Chain"
          link <- expectRight $ mkQualifiedName ["Fixture"] ":>:"
          let parameter = TypeVar 0
              chainType = TypeApp (TypeCons chain) parameter
              extracted = runIdentity
                $ getDataConss Map.empty dataTypes Map.empty [parsedModule]
          case extracted of
            [Right ([binding], DeconstructorBinding input constructors False)] -> do
              input @?= chainType
              binding @?= functionBindingFromType link 0
                (TypeForall [0] []
                  $ TypeArrow parameter
                  $ TypeArrow parameter chainType)
              constructors @?=
                [ConstructorBinding link [parameter, parameter]]
            result -> fail $ "unexpected infix bindings: " ++ show result
      , testCase "field strictness and unpack metadata do not change types" $ do
          parsedModule <- expectParsedModule $ unlines
            [ "{-# LANGUAGE BangPatterns #-}"
            , "module Fixture where"
            , "data Strict"
            , "  = Strict ({-# UNPACK #-} !Strict)"
            , "  | Record { strictField :: (!Strict) }"
            ]
          dataTypes <- expectRight $ getDataTypes [parsedModule]
          strict <- expectRight $ mkQualifiedName ["Fixture"] "Strict"
          record <- expectRight $ mkQualifiedName ["Fixture"] "Record"
          strictField <- expectRight
            $ mkQualifiedName ["Fixture"] "strictField"
          let strictType = TypeCons strict
              extracted = runIdentity
                $ getDataConss Map.empty dataTypes Map.empty [parsedModule]
          case extracted of
            [Right (bindings, DeconstructorBinding input constructors True)] -> do
              input @?= strictType
              bindings @?=
                [ functionBindingFromType strict 0
                    $ TypeArrow strictType strictType
                , functionBindingFromType record 0
                    $ TypeArrow strictType strictType
                , functionBindingFromType strictField 0
                    $ TypeArrow strictType strictType
                ]
              constructors @?=
                [ ConstructorBinding strict [strictType]
                , ConstructorBinding record [strictType]
                ]
            result -> fail $ "unexpected strict-field bindings: " ++ show result
      , testCase "function bindings split quantified signatures once" $ do
          function <- expectRight $ mkQualifiedName ["Fixture"] "function"
          cls <- expectRight $ mkQualifiedName ["Fixture"] "C"
          let constraint = HsConstraint cls [TypeVar 7]
              signature = TypeForall [7] [constraint]
                $ TypeArrow (TypeVar 7) (TypeCons $ name "Int")
          functionBindingFromType function (Penalty 2.5) signature @?=
            FunctionBinding
              (TypeCons $ name "Int")
              function
              (Penalty 2.5)
              [constraint]
              [TypeVar 7]
      , testCase "binding type views cover every stored type once" $ do
          let result = TypeCons $ name "Result"
              parameter = TypeCons $ name "Parameter"
              constrained = TypeCons $ name "Constrained"
              replacement = TypeCons $ name "Replacement"
              constraint = HsConstraint (name "C") [constrained]
              binding = FunctionBinding result (name "binding") 1
                [constraint] [parameter]
              unconstrained = binding { functionConstraints = [] }
              constructor = ConstructorBinding (name "Build")
                [parameter, constrained]
              deconstructor = DeconstructorBinding result
                [constructor] False
              environment = EnvDictionary
                [binding] [deconstructor] emptyStaticClassEnv
          functionBindingType binding @?=
            TypeArrow parameter result
          functionBindingSignature binding @?=
            TypeForall [] [constraint] (TypeArrow parameter result)
          functionBindingSignature unconstrained @?=
            TypeArrow parameter result
          functionBindingTypes binding @?=
            [result, parameter, constrained]
          functionBindingTypes
              (mapFunctionBindingTypes (const replacement) binding) @?=
            replicate 3 replacement
          deconstructorBindingType deconstructor @?=
            TypeArrow parameter (TypeArrow constrained result)
          deconstructorBindingTypes deconstructor @?=
            [result, parameter, constrained]
          deconstructorBindingTypes
              (mapDeconstructorBindingTypes
                (const replacement) deconstructor) @?=
            replicate 3 replacement
          environmentBindingTypes environment @?=
            [ result, parameter, constrained
            , result, parameter, constrained
            ]
      , testCase "function bindings preserve nested forall results" $ do
          function <- expectRight $ mkQualifiedName ["Fixture"] "rankN"
          cls <- expectRight $ mkQualifiedName ["Fixture"] "C"
          let integer = TypeCons $ name "Int"
              nestedConstraint = HsConstraint cls [TypeVar 2]
              nested = TypeForall [2] [nestedConstraint]
                $ TypeArrow (TypeVar 2) (TypeVar 2)
              signature = TypeArrow integer nested
              binding = functionBindingFromType function 0 signature
          binding @?= FunctionBinding nested function 0 [] [integer]
          functionBindingSignature binding @?= signature
          case mkExferenceEnvironment
              $ EnvDictionary [binding] [] emptyStaticClassEnv of
            Left failure -> fail
              $ "a valid rank-N result was rejected: " ++ show failure
            Right _ -> pure ()
      , testCase "empty source foralls canonicalize to monotypes" $ do
          function <- expectRight $ mkQualifiedName ["Fixture"] "identity"
          let body = TypeArrow (TypeVar 0) (TypeVar 0)
              explicitEmpty = TypeForall [] [] body
              binding = functionBindingFromType function 0 explicitEmpty
          binding @?= functionBindingFromType function 0 body
          functionBindingSignature binding @?= body
      , testCase "function bindings preserve prenex lexical shadowing" $ do
          function <- expectRight $ mkQualifiedName ["Fixture"] "shadowed"
          outer <- expectRight $ mkQualifiedName ["Fixture"] "Outer"
          inner <- expectRight $ mkQualifiedName ["Fixture"] "Inner"
          let source = TypeForall [0]
                [HsConstraint outer [TypeVar 0]]
                $ TypeForall [0]
                    [HsConstraint inner [TypeVar 0]]
                $ TypeArrow (TypeVar 0) (TypeVar 0)
          functionBindingFromType function 0 source @?=
            FunctionBinding (TypeVar 1) function 0
              [ HsConstraint outer [TypeVar 0]
              , HsConstraint inner [TypeVar 1]
              ]
              [TypeVar 1]
      , testCase "malformed forall binder lists remain rejectable" $ do
          function <- expectRight $ mkQualifiedName ["Fixture"] "duplicate"
          let malformed = TypeForall [0, 0] [] $ TypeVar 0
              binding = functionBindingFromType function 0 malformed
          binding @?= FunctionBinding malformed function 0 [] []
          case mkExferenceEnvironment
              $ EnvDictionary [binding] [] emptyStaticClassEnv of
            Left failure -> failure @?= InvalidInputType malformed
              (InvalidSynthesisType $ SharedType.DuplicateForallVariable
                $ SharedType.FlexibleVariable 0)
            Right _ -> fail "a duplicate forall binder list was accepted"
      , testCase "datatype recursion follows strongly connected components" $ do
          parsedModule <- expectParsedModule $ unlines
            [ "module Fixture where"
            , "data Direct = Direct [Direct]"
            , "data Tree = Leaf | Branch Forest"
            , "data Forest = Empty | More Tree Forest"
            , "data Acyclic = Wrap (LeafData -> LeafData)"
            , "data LeafData = LeafData"
            , "data Box = Box"
            ]
          dataTypes <- expectRight $ getDataTypes [parsedModule]
          direct <- expectRight $ mkQualifiedName ["Fixture"] "Direct"
          tree <- expectRight $ mkQualifiedName ["Fixture"] "Tree"
          forest <- expectRight $ mkQualifiedName ["Fixture"] "Forest"
          acyclic <- expectRight $ mkQualifiedName ["Fixture"] "Acyclic"
          leafData <- expectRight $ mkQualifiedName ["Fixture"] "LeafData"
          box <- expectRight $ mkQualifiedName ["Fixture"] "Box"
          let extracted = runIdentity
                $ getDataConss Map.empty dataTypes Map.empty [parsedModule]
              headsAndFlags =
                [ (typeConstructorHead input, recursive)
                | Right (_, DeconstructorBinding input _ recursive) <- extracted
                ]
          length extracted @?= 6
          headsAndFlags @?=
            [ (Just direct, True)
            , (Just tree, True)
            , (Just forest, True)
            , (Just acyclic, False)
            , (Just leafData, False)
            , (Just box, False)
            ]
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
      , testCase "scope invariant diagnostics render every alternative" $ do
          show (Scope.MissingScopeId 9 [2, 9])
            @?= "missing scope 9 on parent path 2 -> 9"
          show (Scope.ScopeParentCycle [1, 4, 1])
            @?= "cyclic scope parents: 1 -> 4 -> 1"
          show (Scope.ScopeIdCollision 7)
            @?= "scope allocator attempted to reuse scope 7"
      ]
  , testGroup "type traversal"
      [ testCase "forall substitution avoids capture in context and body" $ do
          let source = TypeForall [1]
                [HsConstraint (name "C") [TypeVar 0, TypeVar 1]]
                (TypeArrow (TypeVar 0) (TypeVar 1))
              substitutions = IntMap.singleton 0 $ TypeVar 1
              expected = TypeForall [2]
                [HsConstraint (name "C") [TypeVar 1, TypeVar 2]]
                (TypeArrow (TypeVar 1) (TypeVar 2))
          applySubstsChecked substitutions source
            @?= Right (Any True, expected)
          applySubst (Subst 0 $ TypeVar 1) source @?= expected
      , testCase "forall substitution protects shadowed keys" $ do
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
      , testCase "substitution freshens every same-name binder in a chain" $ do
          let source = TypeForall [1] []
                $ TypeForall [1] [] $ TypeVar 0
              expected = TypeForall [2] []
                $ TypeForall [3] [] $ TypeVar 1
          applySubsts (IntMap.singleton 0 $ TypeVar 1) source
            @?= (Any True, expected)
      , testCase "substitution is simultaneous rather than recursive" $ do
          let integer = TypeCons $ name "Int"
              source = TypeArrow (TypeVar 0) (TypeVar 1)
              substitutions = IntMap.fromList
                [(0, TypeVar 1), (1, integer)]
          applySubsts substitutions source
            @?= (Any True, TypeArrow (TypeVar 1) integer)
      , testCase "identity substitution retains the historical change flag" $ do
          let source = TypeArrow (TypeVar 0) $ TypeCons $ name "Int"
          applySubsts (IntMap.singleton 0 $ TypeVar 0) source
            @?= (Any True, source)
      , testCase "standalone constraint substitution also avoids capture" $ do
          let source = HsConstraint (name "C")
                [TypeForall [1] [] $ TypeVar 0]
              expected = HsConstraint (name "C")
                [TypeForall [2] [] $ TypeVar 1]
          constraintApplySubstsChecked
              (IntMap.singleton 0 $ TypeVar 1) source
            @?= Right (Any True, expected)
      , testCase "constraint substitution shares freshness across arguments" $ do
          let source = HsConstraint (name "C")
                [ TypeForall [1] [] $ TypeVar 0
                , TypeForall [1] [] $ TypeVar 0
                ]
              expected = HsConstraint (name "C")
                [ TypeForall [2] [] $ TypeVar 1
                , TypeForall [3] [] $ TypeVar 1
                ]
          constraintApplySubstsChecked
              (IntMap.singleton 0 $ TypeVar 1) source
            @?= Right (Any True, expected)
      , testCase "constraint substitution preserves an empty argument list" $ do
          let source = HsConstraint (name "C") []
          constraintApplySubstsChecked
              (IntMap.singleton 0 $ TypeVar 1) source
            @?= Right (Any False, source)
      , testCase "native tuple mapping preserves rigid identities" $ do
          let source = TypeForall [0]
                [HsConstraint (name "C")
                  [TypeTuple Boxed [TypeVar 0, TypeConstant 0]]]
                $ TypeTuple Boxed [TypeVar 0, TypeConstant 0]
              expected = TypeForall [1]
                [HsConstraint (name "C")
                  [TypeTuple Boxed [TypeVar 1, TypeConstant 0]]]
                $ TypeTuple Boxed [TypeVar 1, TypeConstant 0]
          incVarIds (+ 1) source @?= expected
      , testCase "forall normalization retains an unclaimed binder ID" $ do
          let source = TypeForall [7] []
                $ TypeArrow (TypeVar 7) (TypeVar 7)
          alphaNormalizeForalls IntSet.empty source @?=
            Right (source, IntSet.singleton 7)
      , testCase "forall normalization protects a later free identity" $ do
          let source = TypeArrow
                (TypeForall [0] [] $ TypeArrow (TypeVar 0) (TypeVar 0))
                (TypeVar 0)
              expected = TypeArrow
                (TypeForall [1] [] $ TypeArrow (TypeVar 1) (TypeVar 1))
                (TypeVar 0)
          alphaNormalizeForalls IntSet.empty source @?=
            Right (expected, IntSet.fromList [0, 1])
      , testCase "forall normalization traverses tuple elements in order" $ do
          let source = TypeTuple Boxed
                [TypeForall [0] [] $ TypeVar 0, TypeVar 0]
              expected = TypeTuple Boxed
                [TypeForall [1] [] $ TypeVar 1, TypeVar 0]
          alphaNormalizeForalls IntSet.empty source @?=
            Right (expected, IntSet.fromList [0, 1])
      , testCase "forall normalization retains historical external allocation" $ do
          let source = TypeForall [0] [] $ TypeVar 0
              expected = TypeForall [3] [] $ TypeVar 3
          alphaNormalizeForalls (IntSet.fromList [0, 2]) source @?=
            Right (expected, IntSet.fromList [0, 2, 3])
          let boundaryExpected = TypeForall [1] [] $ TypeVar 1
          alphaNormalizeForalls (IntSet.fromList [0, maxBound]) source @?=
            Right (boundaryExpected, IntSet.fromList [0, 1, maxBound])
      , testCase "forall normalization rejects native rigid binders" $ do
          let source = TypeForallNative
                [SharedType.RigidVariable 4] [] $ TypeVar 0
          alphaNormalizeForalls IntSet.empty source @?=
            Left (RigidForallBinderCannotBeNormalized 4)
      , testCase "rigid planning rejects native rigid binders" $ do
          let environment = EnvDictionary [] [] emptyStaticClassEnv
              source = TypeForallNative
                [SharedType.RigidVariable 4] [] $ TypeVar 0
          planRigidInstantiation
              (mkRigidInstantiationContext environment) [] source
            @?= Left (RigidForallBinderCannotBeInstantiated 4)
      , testCase "rigid planning rejects binders at every input site" $ do
          let rigidForall identifier = TypeForallNative
                [SharedType.RigidVariable identifier] [] $ TypeVar 0
              emptyEnvironment = EnvDictionary [] [] emptyStaticClassEnv
              context = mkRigidInstantiationContext emptyEnvironment
              nestedTuple = TypeTuple Boxed
                [TypeVar 1, rigidForall 4]
              extraConstraint = HsConstraint (name "C")
                [TypeApp (TypeCons $ name "Maybe") $ rigidForall 5]
              invalidBinding = FunctionBinding
                (TypeArrow (TypeVar 0) $ rigidForall 6)
                (name "invalid") 0 [] []
              invalidEnvironment = EnvDictionary
                [invalidBinding] [] emptyStaticClassEnv
          planRigidInstantiation context [] nestedTuple
            @?= Left (RigidForallBinderCannotBeInstantiated 4)
          planRigidInstantiation context [extraConstraint] (TypeVar 0)
            @?= Left (RigidForallBinderCannotBeInstantiated 5)
          planRigidInstantiation
              (mkRigidInstantiationContext invalidEnvironment) [] (TypeVar 0)
            @?= Left (RigidForallBinderCannotBeInstantiated 6)
      , testCase "structural tuples retain their nominal head" $ do
          pairName <- expectRight $ SharedName.tupleName Boxed 2
          typeConstructorHead
              (TypeTuple Boxed [TypeVar 0, TypeConstant 0])
            @?= Just pairName
      , testCase "constraint forall inspection follows shared type semantics" $ do
          let plain = HsConstraint (name "Plain") [TypeVar 0]
              quantified = HsConstraint (name "Quantified")
                [TypeForall [1] [] $ TypeVar 1]
          constraintContainsForall plain @?= False
          constraintContainsForall quantified @?= True
      , testCase "arrow splitting stops before a result forall" $ do
          let integer = TypeCons $ name "Int"
              nestedConstraint = HsConstraint (name "C") [TypeVar 2]
              nested = TypeForall [2] [nestedConstraint]
                $ TypeArrow (TypeVar 2) (TypeVar 2)
              source = TypeForall [0] [] $ TypeArrow integer nested
          splitArrowResultParams source @?=
            (nested, [integer], [0], [])
      , testCase "arrow splitting alpha-normalizes prenex shadows" $ do
          let outer = HsConstraint (name "Outer") [TypeVar 0]
              inner = HsConstraint (name "Inner") [TypeVar 0]
              source = TypeForall [0] [outer]
                $ TypeForall [0] [inner]
                $ TypeArrow (TypeVar 0) (TypeVar 0)
          splitArrowResultParams source @?=
            ( TypeVar 1
            , [TypeVar 1]
            , [0, 1]
            , [outer, HsConstraint (name "Inner") [TypeVar 1]]
            )
      , testCase "arrow splitting leaves duplicate binder lists explicit" $ do
          let malformed = TypeForall [0, 0] [] $ TypeVar 0
          splitArrowResultParams malformed @?= (malformed, [], [], [])
      , testCase "arrow splitting after substitution exposes result arrows" $ do
          let integer = TypeCons $ name "Int"
              boolean = TypeCons $ name "Bool"
              substituted = snd $ applySubsts
                (IntMap.singleton 0 $ TypeArrow integer boolean)
                (TypeVar 0)
          splitArrowChain substituted @?= (boolean, [integer])
          splitArrowResultParams substituted @?=
            (boolean, [integer], [], [])
      , testCase "free variables include a forall context" $ do
          let ty = TypeForall [0]
                [HsConstraint (name "C") [TypeVar 0, TypeVar 1]]
                (TypeVar 0)
          freeVars ty @?= Set.singleton 1
      , testCase "forallify binds flexible variables in canonical order" $ do
          let source = TypeForallNative
                [SharedType.FlexibleVariable 2]
                []
                (TypeTuple Boxed
                  [ TypeVar 3
                  , TypeConstant 0
                  , TypeVar 2
                  , TypeVar 1
                  ])
              expected = TypeForallNative
                [ SharedType.FlexibleVariable 1
                , SharedType.FlexibleVariable 3
                , SharedType.FlexibleVariable 2
                ]
                []
                (TypeTuple Boxed
                  [ TypeVar 3
                  , TypeConstant 0
                  , TypeVar 2
                  , TypeVar 1
                  ])
          forallify source @?= expected
      , testCase "largestId includes forall binders and context variables" $ do
          let ty = TypeForall [7]
                [HsConstraint (name "C") [TypeVar 9]] (TypeVar 2)
          largestId ty @?= 9
          maximumFlexibleId ty @?= Just 9
          let negativeOnly =
                TypeArrow (TypeVar (-5)) $ TypeCons $ name "Ground"
          maximumFlexibleId negativeOnly @?= Just (-5)
          largestId negativeOnly @?= -5
          maximumFlexibleId (TypeCons $ name "Ground") @?= Nothing
      , testCase "the full Int domain is a valid flexible-ID domain" $ do
          let identifiers = [minBound, -1, maxBound]
          mapM_ (\identifier -> validateExferenceInput identityInput
              {input_goalType = TypeVar identifier} @?= Right ()) identifiers
          mapM_ (\identifier -> do
              let rendered = showVar identifier
              assertBool ("illegal negative variable rendering: " ++ rendered)
                $ case rendered of
                    initialCharacter : remaining ->
                      initialCharacter >= 'a' && initialCharacter <= 'z'
                      && all (\character ->
                        character >= 'a' && character <= 'z'
                        || character >= '0' && character <= '9'
                        || character == '_') remaining
                    [] -> False
              case parseTypePure rendered of
                Right _ -> pure ()
                Left failure -> fail $ "rendered variable did not parse: "
                  ++ show failure)
            [minBound, -1]
      , testCase "quantified type rendering binds its body spelling" $ do
          let names = Map.fromList [("x", 0), ("y", 1)]
          showHsType names (TypeForall [0] [] $ TypeVar 0)
            @?= "forall x. x"
          showHsType names
            (TypeForall []
              [HsConstraint (name "C") [TypeVar 0]] $ TypeVar 0)
            @?= "(C x) => x"
          showHsType Map.empty (TypeForall [0] [] $ TypeVar 0)
            @?= "forall v0. v0"
          showHsType Map.empty (TypeArrow (TypeVar 0) (TypeVar 1))
            @?= "v0 -> a"
          showHsType Map.empty (TypeCons SharedName.listName) @?= "([])"
          let renderedList = showHsType Map.empty
                (TypeApp (TypeCons SharedName.listName) (TypeVar 0))
          renderedList @?= "[v0]"
          case parseTypePure renderedList of
            Right _ -> pure ()
            Left failure -> fail $ "rendered list did not parse: "
              ++ show failure
          showHsType Map.empty
              (TypeForall [] [] $ TypeArrow (TypeVar 0) (TypeVar 1))
            @?= "v0 -> a"
          showHsConstraint Map.empty (HsConstraint (name "C") [TypeVar 0])
            @?= "C v0"
          let twoBinders = TypeForall [0, 1] []
                $ TypeArrow (TypeVar 0) (TypeVar 1)
              rendered = showHsType names twoBinders
          rendered @?= "forall x y. x -> y"
          case parseTypePure rendered of
            Right _ -> pure ()
            Left failure -> fail $ "rendered forall did not parse: "
              ++ show failure
          let compoundConstraint = HsConstraint (name "C")
                [ TypeApp (TypeCons $ name "Maybe") (TypeVar 0)
                , TypeArrow (TypeVar 0) (TypeVar 1)
                ]
          showHsConstraint names compoundConstraint @?=
            "C (Maybe x) (x -> y)"
          showHsType (Map.fromList [("z", 0), ("a", 0)]) (TypeVar 0)
            @?= "a"
          showHsType (Map.fromList [("!", 0), ("x", 0)]) (TypeVar 0)
            @?= "x"
          showHsType (Map.singleton "_" 0) (TypeVar 0) @?= "v0"
          showHsType Map.empty (TypeConstant 0) @?= "Cv0"
          pairName <- expectRight $ mkBoxedTupleName 2
          showHsType Map.empty
              (TypeApp
                (TypeApp (TypeCons pairName) (TypeVar 0))
                (TypeVar 1))
            @?= "(,) v0 a"
      , testCase "rank-N and impredicative types render round trip" $ do
          forM_
            [ "forall result. result -> result -> result"
            , "forall result. (element -> result -> result) -> result -> result"
            , "(forall result. result -> result -> result) -> "
                ++ "(forall result. "
                ++ "(element -> result -> result) -> result -> result)"
            , "[(forall element. element -> element)]"
            ]
            assertTypeRenderRoundTrip
          (listType, listNames) <- expectRight
            $ parseTypePure "[(forall element. element -> element)]"
          showHsType listNames listType
            @?= "[(forall element. element -> element)]"
      , testCase "rendering avoids lexical source-name capture" $ do
          (shadowed, shadowedNames) <- expectRight $ parseTypePure
            "forall x. (forall x. (x, c)) -> y"
          showHsType shadowedNames shadowed
            @?= "forall x. (forall c'. (c', c)) -> y"

          let reusedIdentifier = TypeForall [0] [] $ TypeArrow
                (TypeVar 0) (TypeForall [0] [] $ TypeVar 0)
          showHsType (Map.singleton "x" 0) reusedIdentifier
            @?= "forall x. x -> forall x'. x'"

          let collisionConstraint = HsConstraint (name "C")
                [TypeForall [3] [] $ TypeTuple Boxed [TypeVar 3, TypeVar 1]]
          showHsConstraint (Map.singleton "c" 1) collisionConstraint
            @?= "C (forall c'. (c', c))"
      , testCase "unifiers compare opaque polytypes by alpha identity" $ do
          let flexibleLeft = TypeForall [0] []
                $ TypeArrow (TypeVar 0) (TypeVar 0)
              flexibleRight = TypeForall [7] []
                $ TypeArrow (TypeVar 7) (TypeVar 7)
              rigidLeft = TypeForallNative
                [SharedType.RigidVariable 0] []
                $ TypeArrow (TypeConstant 0) (TypeConstant 0)
              rigidRight = TypeForallNative
                [SharedType.RigidVariable 7] []
                $ TypeArrow (TypeConstant 7) (TypeConstant 7)
              assertEquivalent left right = do
                unify left right @?= Just (IntMap.empty, IntMap.empty)
                unifyShared left right @?= Just IntMap.empty
                unifyRight left right @?= Just IntMap.empty
                unifyOffset left (HsTypeOffset right 0) @?=
                  Just (IntMap.empty, IntMap.empty)
                unifyRightOffset left (HsTypeOffset right 0) @?=
                  Just IntMap.empty
          assertEquivalent flexibleLeft flexibleRight
          assertEquivalent rigidLeft rigidRight
      , testCase "metavariables bind whole impredicative atoms" $ do
          let polymorphic = TypeForall [0] []
                $ TypeArrow (TypeVar 0) (TypeVar 0)
              listOf element = TypeApp
                (TypeCons SharedName.listName) element
              variable = TypeVar 1
          unify polymorphic variable @?=
            Just (IntMap.empty, IntMap.singleton 1 polymorphic)
          unify variable polymorphic @?=
            Just (IntMap.singleton 1 polymorphic, IntMap.empty)
          unifyShared variable polymorphic @?=
            Just (IntMap.singleton 1 polymorphic)
          unifyRight polymorphic variable @?=
            Just (IntMap.singleton 1 polymorphic)
          unifyRight variable polymorphic @?= Nothing
          unifyShared (listOf variable) (listOf polymorphic) @?=
            Just (IntMap.singleton 1 polymorphic)
      , testCase "opaque polytypes expose only free variables to substitution" $ do
          let integer = TypeCons $ name "Int"
              open = TypeForall [1] []
                $ TypeArrow (TypeVar 1) (TypeVar 0)
              closed = TypeForall [2] []
                $ TypeArrow (TypeVar 2) integer
              pair left right = TypeTuple Boxed [left, right]
          -- The atom equation is deliberately first. It may become equal after
          -- an independent outer equation, but is never decomposed itself.
          unifyShared (pair open $ TypeVar 0) (pair closed integer) @?=
            Just (IntMap.singleton 0 integer)
          unifyShared open closed @?= Nothing
          unifyShared (TypeVar 0) open @?= Nothing
      , testCase "all unifier modes canonicalize structural tuples" $ do
          boxedPairName <- expectRight $ mkBoxedTupleName 2
          unboxedPairName <- expectRight
            $ SharedName.tupleName Unboxed 2
          let integer = TypeCons $ name "Int"
              boolean = TypeCons $ name "Bool"
              tupleApplication constructor = TypeApp
                (TypeApp (TypeCons constructor) integer) boolean
              assertEquivalent structural application = do
                unify structural application @?=
                  Just (IntMap.empty, IntMap.empty)
                unifyShared structural application @?= Just IntMap.empty
                unifyRight structural application @?= Just IntMap.empty
                unifyOffset structural (HsTypeOffset application 0) @?=
                  Just (IntMap.empty, IntMap.empty)
                unifyRightOffset structural (HsTypeOffset application 0) @?=
                  Just IntMap.empty
          assertEquivalent
            (TypeTuple Boxed [integer, boolean])
            (tupleApplication boxedPairName)
          assertEquivalent
            (TypeTuple Unboxed [integer, boolean])
            (tupleApplication unboxedPairName)
          -- A unary unboxed tuple is a valid structural type but deliberately
          -- has no constructor-application spelling.
          assertEquivalent
            (TypeTuple Unboxed [integer])
            (TypeTuple Unboxed [integer])
      , testCase "unifiers reject invalid or incompatible tuple shapes" $ do
          let integer = TypeCons $ name "Int"
              malformedBoxed = TypeTuple Boxed [integer]
              oversized = TypeTuple Unboxed
                $ replicate (SharedName.maximumTupleArity + 1) integer
              boxedPair = TypeTuple Boxed [integer, integer]
              unboxedPair = TypeTuple Unboxed [integer, integer]
              boxedTriple = TypeTuple Boxed [integer, integer, integer]
              assertRejected left right = do
                unify left right @?= Nothing
                unifyShared left right @?= Nothing
                unifyRight left right @?= Nothing
                unifyOffset left (HsTypeOffset right 0) @?= Nothing
                unifyRightOffset left (HsTypeOffset right 0) @?= Nothing
          mapM_ (uncurry assertRejected)
            [ (malformedBoxed, malformedBoxed)
            , (oversized, oversized)
            , (boxedPair, unboxedPair)
            , (boxedPair, boxedTriple)
            ]
      , testCase "tuple substitutions stay structural and higher-kinded" $ do
          pairName <- expectRight $ mkBoxedTupleName 2
          let integer = TypeCons $ name "Int"
              boolean = TypeCons $ name "Bool"
              pair = TypeTuple Boxed [integer, boolean]
              elementPattern = TypeTuple Boxed [TypeVar 1, boolean]
              constructorPattern = TypeApp
                (TypeApp (TypeVar 0) integer) boolean
          unifyRight pair (TypeVar 2) @?=
            Just (IntMap.singleton 2 pair)
          unifyRight pair elementPattern @?=
            Just (IntMap.singleton 1 integer)
          unifyShared pair elementPattern @?=
            Just (IntMap.singleton 1 integer)
          unify pair constructorPattern @?=
            Just (IntMap.empty, IntMap.singleton 0 $ TypeCons pairName)
          unifyShared pair constructorPattern @?=
            Just (IntMap.singleton 0 $ TypeCons pairName)
          unifyRight pair constructorPattern @?=
            Just (IntMap.singleton 0 $ TypeCons pairName)
      , testCase "right unification separates colliding side identifiers" $ do
          let pair pairParameter pairResult = TypeApp
                (TypeApp (TypeCons $ name "Pair") pairParameter) pairResult
              integer = TypeCons $ name "Int"
              left = pair (TypeVar 1) integer
              right = pair (TypeVar 0) (TypeVar 1)
          unifyRight left right @?= Just (IntMap.fromList
            [(0, TypeVar 1), (1, integer)])
          assertRightUnifierCloses left right
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
      , testCase "shared unification rejects a recursive scoped variable" $ do
          let variable = TypeVar 0
              recursive = TypeApp (TypeCons $ name "F") variable
          -- The ordinary symmetric unifier deliberately gives its two inputs
          -- independent namespaces, so the equal spelling is not a cycle.
          assertBool "disjoint namespaces unexpectedly shared variable 0"
            $ case unifyDisjoint variable recursive of
                Just _ -> True
                Nothing -> False
          unifyShared variable recursive @?= Nothing
      , testCase "symmetric unification allocates across the Int boundary" $ do
          let apply constructor arguments = foldl TypeApp
                (TypeCons $ name constructor) arguments
              left = apply "Triple"
                [TypeVar 0, TypeVar maxBound, TypeVar minBound]
              right = apply "Triple"
                [ apply "F" [TypeVar maxBound]
                , TypeCons $ name "A"
                , TypeCons $ name "B"
                ]
          case unify left right of
            Nothing -> fail "boundary unification unexpectedly failed"
            Just (leftSubstitutions, _) -> do
              assertUnifierCloses left right
              case IntMap.lookup 0 leftSubstitutions of
                Just (TypeApp (TypeCons constructor) (TypeVar allocated)) -> do
                  constructor @?= name "F"
                  assertBool "right variable captured a left boundary ID"
                    $ allocated `notElem` [0, minBound, maxBound]
                replacement -> fail $ "unexpected boundary substitution: "
                  ++ show replacement
      , testCase "offset unifiers reject arithmetic overflow" $ do
          let overflowing = HsTypeOffset (TypeVar 1) maxBound
          unifyOffset (TypeVar 0) overflowing @?= Nothing
          unifyRightOffset (TypeVar 0) overflowing @?= Nothing
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
          mapM_ (uncurry assertSharedUnifierCloses) pairs
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
      , testCase "raw synonym parameters are distinct when reached" $ do
          let alias = name "Duplicate"
              integer = TypeCons $ name "Int"
              boolean = TypeCons $ name "Bool"
              declaration = HsTypeDecl alias [0, 1, 0, 1] (TypeVar 0)
              declarations = Map.singleton alias $ Right declaration
              saturated = foldl TypeApp (TypeCons alias)
                [integer, boolean, boolean, integer]
              expected = Left
                "duplicate parameter 0 for type declaration Duplicate"
          -- The duplicate beats both the bare application's arity failure and
          -- the saturated application's historically lossy substitution.
          applyTypeDecls declarations (TypeCons alias) @?= expected
          applyTypeDecls declarations saturated @?= expected
          applyTypeDecls declarations integer @?= Right integer
      , testCase "legacy synonym adaptation avoids forall capture" $ do
          let alias = name "Capture"
              typeClass = name "C"
              declaration = HsTypeDecl alias [0]
                $ TypeForall [1] [HsConstraint typeClass [TypeVar 0]]
                $ TypeArrow (TypeVar 0) (TypeVar 1)
              applied = TypeApp (TypeCons alias) (TypeVar 1)
          applyTypeDecls (Map.singleton alias $ Right declaration) applied @?=
            Right (TypeForall [2] [HsConstraint typeClass [TypeVar 1]]
              $ TypeArrow (TypeVar 1) (TypeVar 2))
      , testCase "shared conversion canonicalizes flexible, rigid, tuple, and forall forms" $ do
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
          fromSynthesisType shared @?= Right shared
      , testCase "shared unboxed tuple types lower without narrowing names" $
          mapM_ (\shared -> do
              source <- expectRight $ fromSynthesisType shared
              toSynthesisType source @?= Right shared)
            [ SharedType.TupleType SharedName.Unboxed []
            , SharedType.TupleType SharedName.Unboxed
                [ SharedType.TypeVariable $ SharedType.FlexibleVariable 0
                , SharedType.TypeVariable $ SharedType.RigidVariable 7
                ]
            ]
      , testCase "shared type conversion rejects malformed class names" $ do
          let invalidName = name "constraint"
              source = TypeForall [0]
                [HsConstraint invalidName [TypeVar 0]]
                (TypeVar 0)
          toSynthesisType source @?= Left
            (InvalidSynthesisType $ SharedType.InvalidTypeConstraint
              $ SharedConstraint.InvalidConstraintClass
              invalidName)
      , testCase "shared rigid forall binders are rejected" $ do
          let malformed = SharedType.ForallType
                [SharedType.RigidVariable 4] []
                (SharedType.TypeVariable $ SharedType.RigidVariable 4)
          fromSynthesisType malformed @?= Left (RigidForallBinder 4)
      , testCase "shared binder errors precede rigid-binder rejection" $ do
          let duplicate = SharedType.RigidVariable 4
              malformed = SharedType.ForallType [duplicate, duplicate] []
                $ SharedType.TypeVariable duplicate
          fromSynthesisType malformed @?= Left
            (InvalidSynthesisType
              $ SharedType.DuplicateForallVariable duplicate)
      ]
  , testGroup "shared declarations"
      [ testCase "function bindings preserve penalties and constraints" $ do
          let constraint = HsConstraint (name "C") [TypeVar 0]
              binding = FunctionBinding
                (TypeVar 0) (name "mapOne") (Penalty 2.5)
                [constraint] [TypeArrow (TypeVar 0) (TypeVar 0)]
          shared <- expectRight $ toSynthesisFunctionBinding binding
          fromSynthesisFunctionBinding shared @?= Right binding
      , testCase "unconstrained bindings use canonical shared monotypes" $ do
          let function = name "identity"
              body = TypeArrow (TypeVar 0) (TypeVar 0)
              binding = FunctionBinding
                (TypeVar 0) function (Penalty 1.5) [] [TypeVar 0]
              compatibilityDeclaration = SharedDeclaration.ValueDeclaration
                $ SharedDeclaration.ValueSignature
                    (SearchPenaltyMetadata $ Penalty 1.5)
                    function
                    (TypeForall [] [] body)
          shared <- expectRight $ toSynthesisFunctionBinding binding
          case shared of
            SharedDeclaration.ValueDeclaration signature ->
              SharedDeclaration.valueType signature @?= body
            _ -> fail "function adapter returned another declaration shape"
          fromSynthesisFunctionBinding compatibilityDeclaration @?=
            Right binding
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
      , testCase "class methods preserve owner, type, and penalty exactly" $ do
          let className = name "C"
              methodName = name "method"
              classDeclaration = HsTypeClass className [7] []
              ownerConstraint = classMethodConstraint classDeclaration
              method = FunctionBinding
                (TypeVar 7) methodName (Penalty 2.5)
                [ownerConstraint] [TypeVar 7]
          shared <- expectRight $ toSynthesisClassDeclarationWithMethods
            classDeclaration [method]
          case shared of
            SharedDeclaration.ClassDeclaration _ _ _ _ [signature] -> do
              SharedDeclaration.valueName signature @?=
                methodName
              SharedDeclaration.valueAnnotation signature @?=
                SearchPenaltyMetadata (Penalty 2.5)
              SharedDeclaration.valueType signature @?=
                TypeArrow (TypeVar 7) (TypeVar 7)
            _ -> fail "class-method adapter returned another declaration shape"
          fromSynthesisClassDeclarationWithMethods shared @?=
            Right (classDeclaration, [method])
      , testCase "class method constraints preserve native forall binders" $ do
          let owner = HsConstraint (name "Owner") [TypeVar 0]
              inherited = HsConstraint (name "Inherited") [TypeConstant 7]
              binders = [SharedType.RigidVariable 7]
              body = TypeArrow (TypeConstant 7) (TypeVar 0)
              source = TypeForallNative binders [inherited] body
          addClassMethodConstraint owner source @?=
            TypeForallNative binders [owner, inherited] body
      , testCase "class method constraint failures report both sides" $ do
          let className = name "C"
              methodName = name "method"
              classDeclaration = HsTypeClass className [0] []
              expected = classMethodConstraint classDeclaration
              actual = HsConstraint className [TypeCons $ name "Int"]
              binding constraints = FunctionBinding
                (TypeVar 0) methodName (Penalty 1) constraints [TypeVar 0]
          toSynthesisClassDeclarationWithMethods classDeclaration
              [binding [actual]] @?= Left
            (MismatchedClassMethodConstraint methodName expected actual)
          toSynthesisClassDeclarationWithMethods classDeclaration
              [binding []] @?= Left
            (MissingClassMethodConstraint methodName)
      , testCase "deconstructor records preserve data shape and recursion" $ do
          let input = TypeApp (TypeCons $ name "Maybe") (TypeVar 0)
              declaration = DeconstructorBinding input
                [ ConstructorBinding (name "Nothing") []
                , ConstructorBinding (name "Just") [TypeVar 0]
                ] True
          shared <- expectRight $ toSynthesisDataDeclaration declaration
          fromSynthesisDataDeclaration shared @?= Right declaration
      , testCase "datatype heads bound poisoned and cyclic tuple spines" $ do
          let integer = TypeCons $ name "Int"
              observedWidth = SharedName.maximumTupleArity + 1
              poisonousElements =
                replicate observedWidth integer
                  ++ error "oversized tuple tail was inspected"
              poisonous = DeconstructorBinding
                (TypeTuple Boxed poisonousElements) [] True
              expectedWidthFailure = Left
                $ DeclarationTypeConversionError
                $ InvalidSynthesisType
                $ SharedType.InvalidTupleTypeArity Boxed observedWidth
          toSynthesisDataDeclaration poisonous @?= expectedWidthFailure

          -- Recursion derivation is intentionally best-effort over legacy
          -- records. A malformed head is omitted from its graph, but that
          -- omission must still be finite for a cyclic raw list spine.
          let cyclicElements = integer : cyclicElements
              cyclic = DeconstructorBinding
                (TypeTuple Boxed cyclicElements) [] True
          case deriveRecursiveDataMetadata [cyclic] of
            [classified] -> deconstructorRecursive classified @?= False
            _ -> fail "recursion derivation changed the declaration count"
      , testCase "datatype heads reject duplicate forall binders" $ do
          let variable = SharedType.FlexibleVariable 0
              typeName = name "Recursive"
              applied = TypeApp (TypeCons typeName) (TypeVar 0)
              malformedHead = TypeForall [0, 0] [] applied
              declaration = DeconstructorBinding malformedHead
                [ConstructorBinding (name "Recursive") [applied]] True
              expectedFailure = Left
                $ DeclarationTypeConversionError
                $ InvalidSynthesisType
                $ SharedType.DuplicateForallVariable variable
          toSynthesisDataDeclaration declaration @?= expectedFailure
          map deconstructorRecursive
            (deriveRecursiveDataMetadata [declaration]) @?= [False]
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
            (MissingSearchPenaltyMetadata $ name "Nothing")
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
      , testCase "core type synonym lowering owns compatibility policy" $ do
          let synonymName = name "Alias"
              variable = SharedType.FlexibleVariable 0
              explicitParameter = SharedDeclaration.TypeParameter variable
                $ Just SharedKind.ProperTypeKind
              rigidParameter = SharedDeclaration.TypeParameter
                (SharedType.RigidVariable 1) Nothing
              synonym parameter = SharedDeclaration.TypeSynonymDeclaration
                NoDeclarationMetadata synonymName [parameter]
                (SharedType.TypeVariable
                  $ SharedDeclaration.parameterVariable parameter)
          fromSynthesisTypeSynonym (synonym explicitParameter) @?=
            Left (ExplicitParameterKindUnsupported variable)
          fromSynthesisTypeSynonym (synonym rigidParameter) @?=
            Left (RigidDataParameter 1)
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
            @?= [name "answer"]
          Map.keys (SharedEnvironment.classDeclarationMap shared)
            @?= [name "C"]
          Map.size (SharedEnvironment.instanceDeclarationMap shared) @?= 1
          lowered <- expectRight $ fromSynthesisEnvironment shared
          environmentFunctions lowered @?= [function]
          environmentDeconstructors lowered @?= [deconstructor]
          sClassEnv_tclasses (environmentClasses lowered)
            @?= Map.singleton (name "C") cls
          sClassEnv_explicitInstances (environmentClasses lowered)
            @?= [instanceDeclaration]
      , testCase "rich core environments round-trip nested method ownership" $ do
          let className = name "C"
              classDeclaration = HsTypeClass className [0] []
              ownerConstraint = classMethodConstraint classDeclaration
              ordinary = FunctionBinding
                (TypeCons $ name "Int") (name "ordinary")
                (Penalty 1.5) [] []
              method = FunctionBinding
                (TypeVar 0) (name "method") (Penalty 2.5)
                [ownerConstraint] [TypeVar 0]
              secondMethod = FunctionBinding
                (TypeVar 0) (name "secondMethod") (Penalty 3.5)
                [ownerConstraint] []
          classes <- expectRight $ mkStaticClassEnv [classDeclaration] []
          shared <- expectRight
            $ toSynthesisEnvironmentWithConstructorPenaltiesAndClassMethods
                Map.empty
                (Map.singleton className [method, secondMethod])
                (EnvDictionary [ordinary] [] classes)
          Set.fromList (Map.keys $ SharedEnvironment.valueSignatureMap shared)
            @?= Set.fromList (map functionName
              [method, secondMethod, ordinary])
          case Map.lookup className
              (SharedEnvironment.classDeclarationMap shared) of
            Just (SharedDeclaration.ClassDeclaration _ _ _ _ methods) ->
              map SharedDeclaration.valueName methods @?=
                map functionName [method, secondMethod]
            declaration -> fail $ "nested class missing: " ++ show declaration
          case fromSynthesisEnvironment shared of
            Left failure -> failure @?=
              SynthesisEnvironmentDeclarationError (ClassMethodsUnsupported
                $ map functionName [method, secondMethod])
            Right _ -> fail "legacy lowering erased class-method ownership"
          (lowered, loweredMethods) <- expectRight
            $ fromSynthesisEnvironmentWithClassMethods shared
          environmentFunctions lowered @?= [ordinary]
          sClassEnv_tclasses (environmentClasses lowered) @?=
            Map.singleton className classDeclaration
          loweredMethods @?= Map.singleton className [method, secondMethod]
          raised <- expectRight
            $ toSynthesisEnvironmentWithConstructorPenaltiesAndClassMethods
                Map.empty loweredMethods lowered
          raised @?= shared
      , testCase "core lowering rejects frontend-only declarations" $ do
          let synonymName = name "Alias"
              synonym = SharedDeclaration.TypeSynonymDeclaration
                NoDeclarationMetadata synonymName []
                (SharedType.TypeConstructor $ name "Int")
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
                NoDeclarationMetadata (name "method")
                (SharedType.TypeVariable $ SharedType.FlexibleVariable 0)
              sharedClass = SharedDeclaration.ClassDeclaration
                NoDeclarationMetadata (name "C")
                [parameter] [] [method]
          fromSynthesisClassDeclaration sharedClass @?=
            Left (ClassMethodsUnsupported [name "method"])
          toSynthesisDataDeclaration
              (DeconstructorBinding
                (TypeApp (TypeCons $ name "T") (TypeCons $ name "Int"))
                [] False)
            @?= Left (NonVariableDataParameter $ TypeCons $ name "Int")
          let unused = SharedType.FlexibleVariable 9
              sharedInstance = SharedDeclaration.InstanceDeclaration
                NoDeclarationMetadata [unused] []
                (SharedConstraint.Constraint
                  (name "C") [])
          fromSynthesisInstanceDeclaration sharedInstance @?=
            Left (NonImplicitInstanceForall [unused])
      ]
  , testGroup "neutral shared environment lowering"
      [ testCase "fresh variables cover the complete tagged Int domain" $ do
          let flexible = SharedType.FlexibleVariable
              rigid = SharedType.RigidVariable
          -- Allocation is gap-filling, not max-plus-one. This matters after
          -- moving the supply below both declaration and type operations.
          freshSynthesisVariable (Set.singleton $ flexible 7) (flexible 7)
            @?= Just (flexible 0)
          freshSynthesisVariable
              (Set.fromList [flexible maxBound, flexible minBound])
              (flexible maxBound)
            @?= Just (flexible 0)
          freshSynthesisVariable
              (Set.fromList [rigid 0, rigid 1, rigid maxBound, rigid minBound])
              (rigid minBound)
            @?= Just (rigid 2)
          -- Tags are distinct identity spaces even when the numeric payload
          -- is equal.
          freshSynthesisVariable (Set.singleton $ rigid 0) (flexible maxBound)
            @?= Just (flexible 0)
      , testCase "ordinary values lower with zero penalty" $ do
          let variable = neutralVariable (-7)
              identityName = neutralName "identity"
              identityType = SharedType.FunctionType
                (SharedType.TypeVariable variable)
                (SharedType.TypeVariable variable)
          environment <- lowerNeutralDeclarations
            [neutralValue identityName identityType]
          environmentFunctions environment @?=
            [ FunctionBinding (TypeVar 0) (name "identity") 0 [] [TypeVar 0]
            ]
          environmentDeconstructors environment @?= []
          sClassEnv_tclasses (environmentClasses environment) @?= Map.empty
      , testCase "explicit kinds and negative class IDs stay in the inventory" $ do
          let className = neutralName "Higher"
              methodName = neutralName "lifted"
              parameterVariable = neutralVariable minBound
              parameter = SharedDeclaration.TypeParameter parameterVariable
                $ Just $ SharedKind.FunctionKind
                    SharedKind.ProperTypeKind SharedKind.ProperTypeKind
              integer = SharedType.TypeConstructor $ neutralName "Int"
              applied = SharedType.TypeApplication
                (SharedType.TypeVariable parameterVariable) integer
              method = SharedDeclaration.ValueSignature () methodName
                $ SharedType.FunctionType applied applied
              classDeclaration = SharedDeclaration.ClassDeclaration () className
                [parameter] [] [method]
              -- The explicit unused binder is legal in the neutral IR but
              -- absent from Exference's implicit instance representation.
              unused = neutralVariable (-99)
              instanceDeclaration = SharedDeclaration.InstanceDeclaration ()
                [unused] [] $ SharedConstraint.Constraint className
                  [SharedType.TypeConstructor SharedName.listName]
          environment <- lowerNeutralDeclarations
            [classDeclaration, instanceDeclaration]
          let classes = environmentClasses environment
              owner = HsConstraint (name "Higher") [TypeVar 0]
              appliedCore = TypeApp (TypeVar 0) (TypeCons $ name "Int")
          sClassEnv_tclasses classes @?= Map.singleton (name "Higher")
            (HsTypeClass (name "Higher") [0] [])
          environmentFunctions environment @?=
            [ FunctionBinding appliedCore (name "lifted") 0
                [owner] [appliedCore]
            ]
          sClassEnv_explicitInstances classes @?=
            [ HsInstance []
                $ HsConstraint (name "Higher") [TypeCons ListCon]
            ]
      , testCase "only complete leading forall chains become implicit" $ do
          let className = neutralName "C"
              contextVariable = neutralVariable (-10)
              nestedVariable = neutralVariable maxBound
              integer = SharedType.TypeConstructor $ neutralName "Int"
              cls = SharedDeclaration.ClassDeclaration () className
                [neutralParameter contextVariable] [] []
              prenex = SharedType.ForallType [contextVariable]
                [SharedConstraint.Constraint className
                  [SharedType.TypeVariable contextVariable]]
                $ SharedType.FunctionType
                    (SharedType.TypeVariable contextVariable)
                    (SharedType.TypeVariable contextVariable)
              nested = SharedType.FunctionType
                (SharedType.ForallType [nestedVariable] []
                  $ SharedType.TypeVariable nestedVariable)
                integer
          environment <- lowerNeutralDeclarations
            [ cls
            , neutralValue (neutralName "prenex") prenex
            , neutralValue (neutralName "nested") nested
            ]
          environmentFunctions environment @?=
            [ FunctionBinding (TypeVar 1) (name "prenex") 0
                [HsConstraint (name "C") [TypeVar 1]] [TypeVar 1]
            , FunctionBinding (TypeCons $ name "Int") (name "nested") 0 []
                [TypeForall [0] [] $ TypeVar 0]
            ]
      , testCase "implicit forall flattening preserves lexical shadowing" $ do
          let variable = neutralVariable (-1)
              classDeclaration className =
                SharedDeclaration.ClassDeclaration () className
                  [neutralParameter variable] [] []
              constraint className = SharedConstraint.Constraint className
                [SharedType.TypeVariable variable]
              standalone = SharedType.ForallType [variable]
                [constraint $ neutralName "Outer"]
                $ SharedType.ForallType [variable]
                    [constraint $ neutralName "Inner"]
                    $ SharedType.FunctionType
                        (SharedType.TypeVariable variable)
                        (SharedType.TypeVariable variable)
              method = SharedDeclaration.ValueSignature ()
                (neutralName "method")
                $ SharedType.ForallType [variable] []
                $ SharedType.FunctionType
                    (SharedType.TypeVariable variable)
                    (SharedType.TypeVariable variable)
              owner = SharedDeclaration.ClassDeclaration ()
                (neutralName "Owner") [neutralParameter variable] [] [method]
          environment <- lowerNeutralDeclarations
            [ classDeclaration $ neutralName "Outer"
            , classDeclaration $ neutralName "Inner"
            , neutralValue (neutralName "standalone") standalone
            , owner
            ]
          environmentFunctions environment @?=
            [ FunctionBinding (TypeVar 2) (name "standalone") 0
                [ HsConstraint (name "Outer") [TypeVar 1]
                , HsConstraint (name "Inner") [TypeVar 2]
                ]
                [TypeVar 2]
            , FunctionBinding (TypeVar 1) (name "method") 0
                [HsConstraint (name "Owner") [TypeVar 0]]
                [TypeVar 1]
            ]
      , testCase "constructors, methods, and instances preserve inventory order" $ do
          let integer = SharedType.TypeConstructor $ neutralName "Int"
              boolean = SharedType.TypeConstructor $ neutralName "Bool"
              dataVariable = neutralVariable (-3)
              classVariable = neutralVariable (-4)
              boxName = neutralName "Box"
              className = neutralName "C"
              boxDeclaration = SharedDeclaration.DataTypeDeclaration () boxName
                [neutralParameter dataVariable]
                [ SharedDeclaration.DataConstructor () (neutralName "Empty") []
                , SharedDeclaration.DataConstructor () (neutralName "Boxed")
                    [SharedType.TypeVariable dataVariable]
                ]
              classDeclaration = SharedDeclaration.ClassDeclaration () className
                [neutralParameter classVariable] []
                [ SharedDeclaration.ValueSignature () (neutralName "methodOne")
                    $ SharedType.TypeVariable classVariable
                , SharedDeclaration.ValueSignature () (neutralName "methodTwo")
                    $ SharedType.TypeVariable classVariable
                ]
              instanceFor typeExpression =
                SharedDeclaration.InstanceDeclaration () [] []
                  $ SharedConstraint.Constraint className [typeExpression]
          environment <- lowerNeutralDeclarations
            [ neutralValue (neutralName "first") integer
            , boxDeclaration
            , classDeclaration
            , instanceFor integer
            , instanceFor boolean
            , neutralValue (neutralName "last") boolean
            ]
          map functionName (environmentFunctions environment) @?=
            map name
              [ "first", "Empty", "Boxed"
              , "methodOne", "methodTwo", "last"
              ]
          assertBool "a neutral binding retained a nonzero penalty"
            $ all ((== 0) . functionPenalty) $ environmentFunctions environment
          case environmentDeconstructors environment of
            [DeconstructorBinding input constructors False] -> do
              input @?= TypeApp (TypeCons $ name "Box") (TypeVar 0)
              map constructorName constructors @?=
                map name ["Empty", "Boxed"]
            deconstructors -> fail $ "unexpected deconstructors: "
              ++ show deconstructors
          sClassEnv_explicitInstances (environmentClasses environment) @?=
            [ HsInstance [] $ HsConstraint (name "C")
                [TypeCons $ name "Int"]
            , HsInstance [] $ HsConstraint (name "C")
                [TypeCons $ name "Bool"]
            ]
      , testCase "general preparation retains arbitrary annotations" $ do
          let declaration
                :: SharedDeclaration.Declaration
                    SynthesisVariable Void String
              declaration = SharedDeclaration.AbstractTypeDeclaration
                "presentation metadata" (neutralName "Opaque")
                SharedKind.ProperTypeKind
          inventory <- expectRight $ SharedInventory.mkInventory
            SharedKindInference.OpenKindInventory [declaration]
          prepared <- expectRight $ prepareSynthesisInventory inventory
          SharedTypeSynonym.preparedInventory
              (preparedSynthesisWitness prepared) @?= inventory
      , testCase "prepared projections check bindings before source data heads" $ do
          let variable = neutralVariable 0
              identityType = SharedType.FunctionType
                (SharedType.TypeVariable variable)
                (SharedType.TypeVariable variable)
              declaration spelling = neutralValue
                (neutralName spelling) identityType
          leftInventory <- expectRight $ SharedInventory.mkInventory
            SharedKindInference.OpenKindInventory [declaration "left"]
          rightInventory <- expectRight $ SharedInventory.mkInventory
            SharedKindInference.OpenKindInventory [declaration "right"]
          leftPrepared <- expectRight
            $ prepareSynthesisInventory leftInventory
          rightPrepared <- expectRight
            $ prepareSynthesisInventory rightInventory
          let unrelatedView =
                [ (functionName binding, functionPenalty binding)
                | binding <- environmentFunctions
                    $ preparedSynthesisBackend rightPrepared
                ]
              malformedDeconstructor = DeconstructorBinding
                (TypeVar 17) [] False
          case projectSynthesisInventory
              unrelatedView [malformedDeconstructor] leftPrepared of
            Left failure -> failure @?= PreparedBindingNamesMismatch
              [name "right"] [name "left"]
            Right _ -> fail
              "an unrelated backend view was attached to a checked inventory"
          let matchingView =
                [ (functionName binding, functionPenalty binding)
                | binding <- environmentFunctions
                    $ preparedSynthesisBackend leftPrepared
                ]
          case projectSynthesisInventory
              matchingView [malformedDeconstructor] leftPrepared of
            Left failure -> failure @?= SynthesisEnvironmentDeclarationError
              (InvalidDeconstructorHead $ TypeVar 17)
            Right _ -> fail "a malformed source datatype head was projected"
      , testCase "prepared projections preserve exact order, ratings, and shapes" $ do
          let dataDeclaration
                :: String
                -> String
                -> SharedDeclaration.Declaration SynthesisVariable Void ()
              dataDeclaration typeSpelling constructorSpelling =
                SharedDeclaration.DataTypeDeclaration ()
                  (neutralName typeSpelling) []
                  [ SharedDeclaration.DataConstructor ()
                      (neutralName constructorSpelling) []
                  ]
          inventory <- expectRight $ SharedInventory.mkInventory
            SharedKindInference.OpenKindInventory
            [ dataDeclaration "First" "MakeFirst"
            , dataDeclaration "Second" "MakeSecond"
            ]
          prepared <- expectRight
            $ prepareSynthesisInventory inventory
          let orderedDeconstructors = reverse
                $ environmentDeconstructors
                $ preparedSynthesisBackend prepared
          projected <- expectRight $ projectSynthesisInventory
            [ (name "MakeSecond", Penalty (-2.5))
            , (name "MakeFirst", Penalty 7.25)
            ]
            orderedDeconstructors
            prepared
          let backend = preparedSynthesisBackend projected
          map (\binding ->
              ( functionName binding
              , functionPenalty binding
              , functionResult binding
              )) (environmentFunctions backend) @?=
            [ (name "MakeSecond", Penalty (-2.5), TypeCons $ name "Second")
            , (name "MakeFirst", Penalty 7.25, TypeCons $ name "First")
            ]
          map (typeConstructorHead . deconstructorInput)
              (environmentDeconstructors backend) @?=
            map (Just . name) ["Second", "First"]
      , testCase "prepared projections reject non-finite source ratings" $ do
          let valueName = neutralName "value"
              notANumber = Penalty $ 0 / 0
          inventory <- expectRight $ SharedInventory.mkInventory
            SharedKindInference.OpenKindInventory
            [ neutralValue valueName
                $ SharedType.TypeConstructor $ neutralName "Int"
            ]
          prepared <- expectRight
            $ prepareSynthesisInventory inventory
          case projectSynthesisInventory
              [(name "value", notANumber)] [] prepared of
            Left failure -> failure @?=
              InvalidPreparedBindingPenalty (name "value") notANumber
            Right _ -> fail "a non-finite source rating was prepared"
      , testCase "alias-expanded direct and mutual recursion is classified" $ do
          let aliasVariable = neutralVariable 70
              phantomVariable = neutralVariable 71
              leftVariable = neutralVariable (-70)
              rightVariable = neutralVariable maxBound
              aliasName = neutralName "Alias"
              phantomName = neutralName "Phantom"
              intName = neutralName "Int"
              leftName = neutralName "LeftRec"
              rightName = neutralName "RightRec"
              erasedName = neutralName "ErasedRec"
              apply constructor variable = SharedType.TypeApplication
                (SharedType.TypeConstructor constructor)
                (SharedType.TypeVariable variable)
              alias = SharedDeclaration.TypeSynonymDeclaration () aliasName
                [neutralParameter aliasVariable]
                $ apply rightName aliasVariable
              phantom = SharedDeclaration.TypeSynonymDeclaration () phantomName
                [neutralParameter phantomVariable]
                $ SharedType.TypeConstructor intName
              left = SharedDeclaration.DataTypeDeclaration () leftName
                [neutralParameter leftVariable]
                [SharedDeclaration.DataConstructor () (neutralName "MkLeft")
                  [apply aliasName leftVariable]]
              right = SharedDeclaration.DataTypeDeclaration () rightName
                [neutralParameter rightVariable]
                [SharedDeclaration.DataConstructor () (neutralName "MkRight")
                  [apply leftName rightVariable]]
              direct = SharedDeclaration.DataTypeDeclaration ()
                (neutralName "DirectRec") []
                [SharedDeclaration.DataConstructor () (neutralName "MkDirect")
                  [SharedType.TypeConstructor $ neutralName "DirectRec"]]
              erased = SharedDeclaration.DataTypeDeclaration () erasedName []
                [SharedDeclaration.DataConstructor () (neutralName "MkErased")
                  [SharedType.TypeApplication
                    (SharedType.TypeConstructor phantomName)
                    (SharedType.TypeConstructor erasedName)]]
          environment <- lowerNeutralDeclarations
            [alias, phantom, left, right, direct, erased]
          map deconstructorRecursive (environmentDeconstructors environment)
            @?= [True, True, True, False]
          map functionName (environmentFunctions environment) @?=
            map name ["MkLeft", "MkRight", "MkDirect", "MkErased"]
      , testCase "unused synonym failures remain preparation errors" $ do
          let variable = neutralVariable 0
              identityName = neutralName "Identity"
              higherName = neutralName "Higher"
              badName = neutralName "UnusedBad"
              identity = SharedDeclaration.TypeSynonymDeclaration ()
                identityName [neutralParameter variable]
                $ SharedType.TypeVariable variable
              higher = SharedDeclaration.AbstractTypeDeclaration () higherName
                $ SharedKind.FunctionKind
                    (SharedKind.FunctionKind SharedKind.ProperTypeKind
                      SharedKind.ProperTypeKind)
                    SharedKind.ProperTypeKind
              bad = SharedDeclaration.TypeSynonymDeclaration () badName []
                $ SharedType.TypeApplication
                    (SharedType.TypeConstructor higherName)
                    (SharedType.TypeConstructor identityName)
          inventory <- expectRight $ SharedInventory.mkInventory
            SharedKindInference.OpenKindInventory [identity, higher, bad]
          case prepareSynthesisInventory inventory of
            Left failure -> failure @?=
              SynonymPreparationError
                (SharedTypeSynonym.UnsaturatedTypeSynonym
                  identityName 1 0)
            Right _ -> fail "an invalid unused synonym reached Exference"
      , testCase "synonym failures retain their owning declaration" $ do
          let aliasVariable = neutralVariable 0
              aliasName = neutralName "Alias"
              higherName = neutralName "Higher"
              alias = SharedDeclaration.TypeSynonymDeclaration () aliasName
                [neutralParameter aliasVariable]
                $ SharedType.TypeVariable aliasVariable
              higher = SharedDeclaration.AbstractTypeDeclaration () higherName
                $ SharedKind.FunctionKind
                    (SharedKind.FunctionKind SharedKind.ProperTypeKind
                      SharedKind.ProperTypeKind)
                    SharedKind.ProperTypeKind
              malformed = neutralValue (neutralName "bad")
                $ SharedType.TypeApplication
                    (SharedType.TypeConstructor higherName)
                    (SharedType.TypeConstructor aliasName)
          inventory <- expectRight $ SharedInventory.mkInventory
            SharedKindInference.OpenKindInventory [alias, higher, malformed]
          case prepareSynthesisInventory inventory of
            Left failure -> failure @?=
              SynonymExpansionError 2 (neutralName "bad")
                (SharedTypeSynonym.UnsaturatedTypeSynonym aliasName 1 0)
            Right _ -> fail "an unsaturated synonym reached Exference search"
      ]
  , testGroup "session rating policy"
      [ testCase "loader diagnostics carry extraction source spans" $
          withTemporaryFile (unlines
            [ "module Located where"
            , "identity :: a -> a"
            , "type Loop = Loop"
            ]) $ \modulePath -> do
              LoadReport result _ <- environmentFromModule modulePath
              failure <- case result of
                Left (TypeDeclarationErrors (failure :| [])) -> pure failure
                Left other -> fail
                  $ "cyclic synonym failed in the wrong phase: " ++ show other
                Right _ -> fail "a cyclic synonym environment was accepted"
              assertBool "historical message text changed"
                $ "cyclic type synonym" `isInfixOf`
                    extractionErrorMessage failure
              -- The failure is attributed to the synonym declaration itself:
              -- line 3 of the loaded module, in the loaded file.
              location <- maybe
                (fail "extraction failure lost its source location") pure
                $ extractionErrorLocation failure
              locationSource location @?= modulePath
              sourceLine (sourceStart $ locationSpan location) @?= 3
              case NonEmpty.toList $ environmentLoadErrorDiagnostics
                  $ TypeDeclarationErrors $ failure :| [] of
                [rendered] -> do
                  diagnosticCode rendered @?= Just "EXF_TYPE_DECLARATION"
                  diagnosticSource rendered @?= Just modulePath
                  fmap (sourceLine . sourceStart) (diagnosticSpan rendered)
                    @?= Just 3
                rendered -> fail
                  $ "unexpected rendered diagnostics: " ++ show rendered
      , testCase "HSE sessions report built-in recursive list elimination" $
          withTemporaryFile (unlines
            [ "module Omissions where"
            , "identity :: a -> a"
            ]) $ \modulePath -> do
              LoadReport result _ <- environmentFromModule modulePath
              checked <- expectRight result
              session <- expectRight
                $ ExferenceSession.mkExferenceSession checked
              case exferenceSessionOmissions session of
                [omission] -> do
                  omittedName omission @?= SharedName.listName
                  omittedReason omission @?=
                    RecursiveDataEliminationUnsupported
                omissions -> fail $ "unexpected HSE omissions: "
                  ++ show omissions
              map diagnosticCode (exferenceSessionDiagnostics session) @?=
                [Just "DJEX_EXF_RECURSIVE_OMISSION"]
      , testCase "HSE sessions preserve and retain rank-N result bindings" $
          withTemporaryFile (unlines
            [ "{-# LANGUAGE RankNTypes #-}"
            , "module RankNResult where"
            , "class C a"
            , "rankN :: Int -> (forall b. C b => b -> b)"
            ]) $ \modulePath -> do
              LoadReport result _ <- environmentFromModule modulePath
              checked <- expectRight result
              function <- expectRight
                $ mkQualifiedName ["RankNResult"] "rankN"
              cls <- expectRight $ mkQualifiedName ["RankNResult"] "C"
              let projection = checkedSourceProjection checked
                  integer = TypeCons $ name "Int"
              _nested <- case find ((== function) . functionName)
                  $ sourceFunctions projection of
                Just binding -> do
                  functionParameters binding @?= [integer]
                  case functionResult binding of
                    quantified@(TypeForall [binder]
                        [HsConstraint actualClass [TypeVar constrained]]
                        (TypeArrow (TypeVar parameter) (TypeVar resultVariable))) -> do
                      actualClass @?= cls
                      constrained @?= binder
                      parameter @?= binder
                      resultVariable @?= binder
                      pure quantified
                    actual -> fail $ "rank-N result was flattened to "
                      ++ show actual
                Nothing -> fail "the checked projection lost RankNResult.rankN"
              let backend = EnvDictionary
                    (sourceFunctions projection)
                    (sourceDeconstructors projection)
                    (sourceClasses projection)
              case mkExferenceEnvironment backend of
                Left failure -> fail
                  $ "the core rejected a checked rank-N projection: "
                  ++ show failure
                Right _ -> pure ()
              session <- expectRight
                $ ExferenceSession.mkExferenceSession checked
              assertBool "the stable session still omitted the rank-N binding"
                $ all ((/= function) . omittedName)
                $ exferenceSessionOmissions session
              let overridePolicy = defaultExferenceSessionPolicy
                    { exferenceRatingOverrides =
                        Map.singleton function $ Penalty 1
                    }
              case ExferenceSession.mkExferenceSessionWithPolicy
                  overridePolicy checked of
                Left failure -> fail
                  $ "an override for a retained rank-N binding failed: "
                  ++ show failure
                Right _ -> pure ()
      , testCase "HSE sessions accept finite signed overrides" $
          withTemporaryFile (unlines
            [ "module Ratings where"
            , "identity :: a -> a"
            ]) $ \modulePath -> do
              LoadReport result _ <- environmentFromModule modulePath
              checked <- expectRight result
              identityName <- expectRight
                $ SharedName.parseName "Ratings.identity"
              let policy = defaultExferenceSessionPolicy
                    { exferenceRatingOverrides =
                        Map.singleton identityName $ Penalty (-3.5)
                    }
              _ <- expectRight
                $ ExferenceSession.mkExferenceSessionWithPolicy policy checked
              pure ()
      , testCase "neutral sessions accept finite overrides" $ do
          let identityName = neutralName "identity"
              variable = neutralVariable 0
              variableType = SharedType.TypeVariable variable
              policy = defaultExferenceSessionPolicy
                { exferenceRatingOverrides =
                    Map.singleton identityName $ Penalty 2.25
                }
          environment <- expectRight $ SharedEnvironment.mkEnvironment
            [neutralValue identityName
              $ SharedType.FunctionType variableType variableType]
          _ <- expectRight $ mkExferenceSessionWithPolicy policy environment
          pure ()
      , testCase "rating overrides reject unavailable names" $ do
          environment <- expectRight $ SharedEnvironment.mkEnvironment
            ([] :: [SharedDeclaration.Declaration SynthesisVariable Void ()])
          let policy = defaultExferenceSessionPolicy
                { exferenceRatingOverrides = Map.singleton
                    (neutralName "missing") $ Penalty 1
                }
          case mkExferenceSessionWithPolicy policy environment of
            Left failure -> do
              diagnosticCode failure @?= Just "DJEX_EXF_POLICY_RATING"
              assertBool ("unexpected policy failure: " ++ show failure)
                $ "unavailable bindings" `isInfixOf` diagnosticMessage failure
            Right _ -> fail "an override for an unavailable binding was accepted"
      , testCase "rating overrides reject excluded bindings" $ do
          let identityName = neutralName "identity"
              variableType = SharedType.TypeVariable $ neutralVariable 0
              policy = defaultExferenceSessionPolicy
                { exferenceExcludedBindings = [identityName]
                , exferenceRatingOverrides =
                    Map.singleton identityName $ Penalty 1
                }
          environment <- expectRight $ SharedEnvironment.mkEnvironment
            [neutralValue identityName
              $ SharedType.FunctionType variableType variableType]
          case mkExferenceSessionWithPolicy policy environment of
            Left failure -> do
              diagnosticCode failure @?= Just "DJEX_EXF_POLICY_RATING"
              assertBool ("unexpected excluded-rating failure: "
                  ++ show failure)
                $ "unavailable bindings" `isInfixOf`
                    diagnosticMessage failure
            Right _ -> fail
              "an override for an excluded binding was accepted"
      , testCase "rating overrides reject NaN" $ do
          let identityName = neutralName "identity"
              variableType = SharedType.TypeVariable $ neutralVariable 0
              policy = defaultExferenceSessionPolicy
                { exferenceRatingOverrides = Map.singleton identityName
                    $ Penalty $ 0 / 0
                }
          environment <- expectRight $ SharedEnvironment.mkEnvironment
            [neutralValue identityName
              $ SharedType.FunctionType variableType variableType]
          case mkExferenceSessionWithPolicy policy environment of
            Left failure -> do
              diagnosticCode failure @?= Just "DJEX_EXF_POLICY_RATING"
              assertBool ("unexpected policy failure: " ++ show failure)
                $ "must be finite" `isInfixOf` diagnosticMessage failure
            Right _ -> fail "a NaN rating override was accepted"
      ]
  , testGroup "sealed search environments"
      [ testCase "share one heuristic default across core and compatibility APIs" $ do
          let historicalDefault = ExferenceHeuristicsConfig
                { heuristics_goalVar = Penalty 4.0
                , heuristics_goalCons = Penalty 0.55
                , heuristics_goalArrow = Penalty 5.0
                , heuristics_goalApp = Penalty 1.9
                , heuristics_stepProvidedGood = Penalty 0.2
                , heuristics_stepProvidedBad = Penalty 5.0
                , heuristics_stepEnvGood = Penalty 6.0
                , heuristics_stepEnvBad = Penalty 22.0
                , heuristics_tempUnusedVarPenalty = Penalty 5.0
                , heuristics_tempMultiVarUsePenalty = Penalty 3.0
                , heuristics_functionGoalTransform = Penalty 0.0
                , heuristics_unusedVar = Penalty 20.0
                , heuristics_solutionLength = Penalty 0.0153
                }
          assertEqual "historical parser-neutral profile"
            historicalDefault defaultHeuristicsConfig
          assertEqual "SimpleDict compatibility re-export"
            defaultHeuristicsConfig SimpleDict.defaultHeuristicsConfig
          assertEqual "stable checked options"
            defaultHeuristicsConfig
            (exferenceHeuristics defaultExferenceOptions)
          assertEqual "complete stable search defaults"
            (ExferenceOptions
              { exferenceAllowUnused = False
              , exferenceAllowResidualConstraints = False
              , exferenceConstraintDeferralSteps = 8192
              , exferenceMultiConstructorPatterns = False
              , exferenceMaximumSteps = 65536
              , exferenceMaximumQueueSize = Just 8192
              , exferenceMaximumDepth = Nothing
              , exferenceHeuristics = defaultHeuristicsConfig
              })
            defaultExferenceOptions
          assertEqual "core/stable options re-export"
            Core.defaultExferenceOptions defaultExferenceOptions
      , testCase "sealed runners preserve complete legacy traces" $ do
          environment <- expectRight $ sealLegacyEnvironment identityInput
          target <- checkedIdentifierTarget "sealedTrace"
          let variable = TypeVar 0
              residualGoal = TypeForall [0]
                [HsConstraint (name "External") [variable]]
                $ TypeArrow variable variable
              residualInput = identityInput
                { input_goalType = residualGoal
                , input_allowConstraints = True
                }
              depthConfig = defaultHeuristicsConfig
                {heuristics_functionGoalTransform = 1}
              variants =
                [ ("identity", identityInput)
                , ("residual constraint", residualInput)
                , ("step limit", identityInput {input_maxSteps = 1})
                , ("queue pruning",
                    identityInput {input_maxQueueSize = Just 0})
                , ("depth pruning", identityInput
                    { input_maxDepth = Just 0
                    , input_heuristicsConfig = depthConfig
                    })
                ]
          mapM_ (\(label, input) -> do
              sourceHints <- expectRight
                $ mkExferenceSourceTypeVariableHints
                    (input_goalType input) (Map.singleton "a" 0)
              legacy <- expectRight $ findExpressionsWithStatsEither input
              canonical <- expectRight $ findQueryResultsInEnvironmentEither
                target sourceHints environment (legacyInputQuery input)
              assertEqual (label ++ " batch count")
                (length legacy) (length canonical)
              sequence_ $ zipWith (assertSameBatch label target)
                legacy canonical)
            variants
      , testCase "validators exactly project checked search preparation" $ do
          environment <- expectRight $ sealLegacyEnvironment identityInput
          target <- checkedIdentifierTarget "validatedPreparation"
          let query = legacyInputQuery identityInput
              invalidHeuristics = defaultHeuristicsConfig
                {heuristics_goalVar = -1}
              duplicate = FunctionBinding
                (TypeVar 0) (name "duplicate") 0 [] []
              invalidInput = identityInput
                { input_maxSteps = 0
                , input_envFuncs = [duplicate, duplicate]
                }
              invalidQuery = mapQueryOptions
                (\options -> options
                  { exferenceMaximumSteps = 0
                  , exferenceHeuristics = invalidHeuristics
                  })
                query
              preparedInput input = ()
                <$ findExpressionsWithStatsEither input
              preparedQuery value = ()
                <$ findQueryResultsInEnvironmentEither
                    target
                    (emptyExferenceSourceTypeVariableHints
                      $ queryGoalType value)
                    environment
                    value
          validateExferenceInput identityInput @?=
            preparedInput identityInput
          validateExferenceInput invalidInput @?= preparedInput invalidInput
          validateExferenceQuery environment query @?= preparedQuery query
          validateExferenceQuery environment invalidQuery @?=
            preparedQuery invalidQuery
      , testCase "query options validate against one sealed environment" $ do
          environment <- expectRight $ sealLegacyEnvironment identityInput
          let query = legacyInputQuery identityInput
              invalidHeuristics = defaultHeuristicsConfig
                {heuristics_goalVar = -1}
              polymorphic = TypeForall [1] [] $ TypeVar 1
              nestedGoal = TypeArrow polymorphic polymorphic
              invalidQueries =
                [ ( "maximum steps"
                  , mapQueryOptions
                      (\options -> options {exferenceMaximumSteps = 0})
                      query
                  , Left $ InvalidMaxSteps 0
                  )
                , ( "constraint deferral"
                  , mapQueryOptions
                      (\options -> options
                        {exferenceConstraintDeferralSteps = -1})
                      query
                  , Left $ InvalidConstraintDeferralSteps (-1)
                  )
                , ( "queue size"
                  , mapQueryOptions
                      (\options -> options
                        {exferenceMaximumQueueSize = Just (-1)})
                      query
                  , Left $ InvalidMaxQueueSize (-1)
                  )
                , ( "search depth"
                  , mapQueryOptions
                      (\options -> options
                        {exferenceMaximumDepth = Just (-1)})
                      query
                  , Left $ InvalidMaxDepth (-1)
                  )
                , ( "heuristics"
                  , mapQueryOptions
                      (\options -> options
                        {exferenceHeuristics = invalidHeuristics})
                      query
                  , Left $ InvalidHeuristic "goalVar" (-1)
                  )
                ]
          mapM_ (\(label, invalidQuery, expected) ->
              assertEqual label expected
                $ validateExferenceQuery environment invalidQuery)
            invalidQueries
          validateExferenceQuery environment
            query {queryGoalType = nestedGoal} @?= Right ()
      , testCase "sealing owns environment-only validation" $ do
          let duplicateName = name "duplicate"
              duplicate = FunctionBinding
                (TypeVar 0) duplicateName 0 [] []
              duplicateEnvironment = legacyInputEnvironment identityInput
                {input_envFuncs = [duplicate, duplicate]}
          case mkExferenceEnvironment duplicateEnvironment of
            Left failure -> failure @?=
              DuplicateFunctionNames [duplicateName]
            Right _ -> fail "duplicate functions reached a sealed environment"

          let bindingName = name "constrained"
              polymorphic = TypeForall [1] [] $ TypeVar 1
              constraint = HsConstraint (name "External") [polymorphic]
              constrained = FunctionBinding
                (TypeVar 0) bindingName 0 [constraint] []
              constrainedEnvironment = legacyInputEnvironment identityInput
                {input_envFuncs = [constrained]}
          case mkExferenceEnvironment constrainedEnvironment of
            Left failure -> fail
              $ "rank-N constraint was rejected: " ++ show failure
            Right _ -> pure ()
      , testCase "target exclusion is exact and absent from metadata" $ do
          let excludedName = name "answer"
          targetName <- expectRight $ SharedName.mkIdentifier "answer"
          target <- expectRight $ Generated.mkDefinitionName targetName
          retainedName <- expectRight
            $ mkQualifiedName ["Other"] "answer"
          let resultType = TypeCons $ name "Bool"
              binding bindingName = FunctionBinding
                { functionResult = resultType
                , functionName = bindingName
                , functionPenalty = 0
                , functionConstraints = []
                , functionParameters = []
                }
              input = identityInput
                { input_goalType = resultType
                , input_envFuncs =
                    [binding excludedName, binding retainedName]
                }
          environment <- expectRight $ sealLegacyEnvironment input
          results <- expectRight
            $ findQueryResultsInEnvironmentEither
                target (emptyExferenceSourceTypeVariableHints resultType)
                environment (legacyInputQuery input)
          let batches = map SharedQuery.resultSearch results
          let outputs =
                [ SharedCandidate.candidateOutput candidate
                | batch <- batches
                , candidate <- SharedSearch.batchCandidates batch
                ]
              usages = map
                (exferenceBindingUsages . SharedSearch.batchMetadata)
                batches
          assertBool "qualified homonym was not retained" $ not $ null outputs
          assertBool "excluded target reached a generated candidate"
            $ all
                (\clause ->
                  Generated.clauseName clause == target
                    && Generated.clauseBody clause
                      == Generated.Global retainedName)
                outputs
          assertBool "candidate evidence was not derived from the payload"
            $ all
                ((== SharedQuery.ValidatedCandidates)
                  . SharedQuery.resultEvidence)
                [ result
                | result <- results
                , not $ null $ SharedSearch.batchCandidates
                    $ SharedQuery.resultSearch result
                ]
          assertBool "excluded target reached binding-usage metadata"
            $ all (Map.notMember excludedName) usages
          assertBool "retained homonym disappeared from binding metadata"
            $ any (Map.member retainedName) usages
      , testCase "pre-existing rigid bindings cannot impersonate query skolems" $ do
          let bindingName = name "rigidValue"
              binding = FunctionBinding
                (TypeConstant 0) bindingName 0 [] []
              goal = TypeForall [1] [] $ TypeVar 1
              input = identityInput
                { input_goalType = goal
                , input_envFuncs = [binding]
                }
              expression = ExpName bindingName
              classes = mkQueryClassEnv emptyClassEnv []
          validateExferenceInput input @?= Right ()
          case findOneExpression input of
            Nothing -> pure ()
            Just _ -> fail "a pre-existing rigid value escaped its identity"
          checkExpression classes [binding] [] goal [] expression @?=
            Left (TypeMismatch (TypeConstant 0) (TypeConstant 1))
      , testCase "sealed plans align C8 across search checker and hints" $ do
          let seedName = name "rigidSeed"
              seed = FunctionBinding (TypeConstant 7) seedName 0 [] []
              goal = TypeForall [0] []
                $ TypeArrow (TypeVar 0) (TypeVar 0)
              input = identityInput
                { input_goalType = goal
                , input_envFuncs = [seed]
                }
              query = legacyInputQuery input
              sourceNames = Map.singleton "source" 0
              identity = ExpLambda 1 (TypeConstant 8)
                $ ExpVar 1 $ TypeConstant 8
          environment <- expectRight $ sealLegacyEnvironment input
          sourceHints <- expectRight
            $ mkExferenceSourceTypeVariableHints goal sourceNames
          target <- checkedIdentifierTarget "rigidPlan"
          checkExpression (mkQueryClassEnv emptyClassEnv []) [seed] []
            goal [] identity @?= Right ()
          results <- expectRight $ findQueryResultsInEnvironmentEither
            target sourceHints environment query
          let candidates = concatMap
                (SharedSearch.batchCandidates . SharedQuery.resultSearch)
                results
          assertBool "C8 identity was filtered by live checking"
            $ not $ null candidates
          case candidates of
            candidate : _ -> do
              let hints = exferenceTypeVariableHints
                    $ SharedCandidate.candidateDetails candidate
              Map.lookup (SharedType.RigidVariable 8) hints @?= Just "source"
              Map.lookup (SharedType.RigidVariable 0) hints @?= Nothing
            [] -> fail "C8 search produced no candidate"
      , testCase "rigid planning handles negatives and leading forall chains" $ do
          let seed = FunctionBinding (TypeConstant (-3))
                (name "negativeSeed") 0 [] []
              environment = EnvDictionary [seed] [] emptyClassEnv
              goal = TypeForall [4] []
                $ TypeForall [9] []
                $ TypeArrow (TypeVar 9) (TypeVar 9)
          plan <- expectRight $ planRigidInstantiation
            (mkRigidInstantiationContext environment) [] goal
          rigidInstantiations plan @?= [(4, 0), (9, 1)]
      , testCase "rigid planning pairs a wide binder layer directly" $ do
          let binders = [0 .. 4095]
              goal = TypeForall binders []
                $ TypeArrow (TypeVar 4095) (TypeVar 4095)
          plan <- expectRight $ planRigidInstantiation
            (mkRigidInstantiationContext
              $ EnvDictionary [] [] emptyClassEnv)
            [] goal
          rigidInstantiations plan @?= zip binders [0 .. 4095]
      , testCase "search freshens negative and boundary namespaces" $ do
          let applied constructor argument = TypeApp
                (TypeCons $ name constructor) argument
              resultType = TypeCons $ name "R"
              sType = TypeCons $ name "S"
              tType = TypeCons $ name "T"
              fName = name "f"
              hName = name "h"
              xName = name "x"
              f = FunctionBinding resultType fName 0 []
                [applied "A" $ TypeVar 0]
              x = FunctionBinding tType xName 0 [] []
              expected = ExpApply (ExpName fName)
                $ ExpApply (ExpName hName) (ExpName xName)
              input goal hVariable = ExferenceInput
                goal
                [ f
                , FunctionBinding (applied "A" sType) hName 0 []
                    [TypeVar hVariable]
                , x
                ]
                [] emptyClassEnv
                False False 0 False 200 Nothing Nothing
                defaultHeuristicsConfig
              cases =
                [ ("negative", input resultType (-1))
                , ( "maxBound"
                  , input (TypeForall [maxBound] [] resultType) 0
                  )
                ]
          mapM_ (\(label, searchInput) -> do
              expressions <- expectRight $ findExpressionsEither searchInput
              assertBool (label ++ " allocator lost f (h x)")
                $ expected `elem` map (\(expression, _, _) -> expression) expressions)
            cases
      , testCase "rigid planning traverses signatures contexts and data fields" $ do
          let constrained = FunctionBinding (TypeVar 0) (name "constrained") 0
                [HsConstraint (name "External") [TypeConstant 12]] []
              deconstructor = DeconstructorBinding
                (TypeCons $ name "Box")
                [ConstructorBinding (name "Box") [TypeConstant 20]] False
              environment = EnvDictionary
                [constrained] [deconstructor] emptyClassEnv
              goal = TypeForall [4]
                [HsConstraint (name "Goal") [TypeConstant 15]]
                $ TypeForall [9] [] $ TypeVar 9
              assumptions =
                [HsConstraint (name "Assumption") [TypeConstant 30]]
          plan <- expectRight $ planRigidInstantiation
            (mkRigidInstantiationContext environment) assumptions goal
          rigidInstantiations plan @?= [(4, 31), (9, 32)]
      , testCase "rigid boundary planning uses gaps and option errors win" $ do
          let rigidBinding identifier = FunctionBinding
                (TypeConstant identifier) (name "boundary") 0 [] []
              maximalInput = identityInput
                { input_goalType = TypeConstant maxBound
                , input_envFuncs = [rigidBinding maxBound]
                }
              polymorphic = TypeForall [0] [] $ TypeVar 0
              oneBinder = TypeForall [0] []
                $ TypeArrow (TypeVar 0) (TypeVar 0)
              twoBinders = TypeForall [0, 1] []
                $ TypeArrow (TypeVar 1) (TypeVar 1)
          maximalEnvironment <- expectRight
            $ sealLegacyEnvironment maximalInput
          let monomorphicQuery = legacyInputQuery maximalInput
              exhaustedQuery = monomorphicQuery
                {queryGoalType = polymorphic}
              invalidOptions = mapQueryOptions
                (\options -> options {exferenceMaximumSteps = 0})
                exhaustedQuery
          validateExferenceQuery maximalEnvironment monomorphicQuery @?= Right ()
          validateExferenceQuery maximalEnvironment exhaustedQuery @?= Right ()
          validateExferenceQuery maximalEnvironment invalidOptions @?=
            Left (InvalidMaxSteps 0)
          maximalPlan <- expectRight $ planRigidInstantiation
            (mkRigidInstantiationContext
              $ legacyInputEnvironment maximalInput)
            [] polymorphic
          rigidInstantiations maximalPlan @?= [(0, 0)]

          let penultimateInput = identityInput
                { input_goalType = oneBinder
                , input_envFuncs = [rigidBinding (maxBound - 1)]
                }
          penultimateEnvironment <- expectRight
            $ sealLegacyEnvironment penultimateInput
          validateExferenceQuery penultimateEnvironment
            (legacyInputQuery penultimateInput) @?= Right ()
          case findOneExpression penultimateInput of
            Just _ -> pure ()
            Nothing -> fail "the final Int rigid identifier was not usable"
          let twoBinderQuery = (legacyInputQuery penultimateInput)
                {queryGoalType = twoBinders}
          validateExferenceQuery penultimateEnvironment twoBinderQuery @?= Right ()
          penultimatePlan <- expectRight $ planRigidInstantiation
            (mkRigidInstantiationContext
              $ legacyInputEnvironment penultimateInput)
            [] twoBinders
          rigidInstantiations penultimatePlan @?=
            [(0, maxBound), (1, 0)]
          target <- checkedIdentifierTarget "rigidBoundary"
          let boundarySearch = findQueryResultsInEnvironmentEither
                target
                (emptyExferenceSourceTypeVariableHints twoBinders)
                penultimateEnvironment
                twoBinderQuery
          validateExferenceQuery penultimateEnvironment twoBinderQuery @?=
            (() <$ boundarySearch)
          boundaryResults <- expectRight boundarySearch
          assertBool "the retained boundary plan produced no candidate"
            $ not
            $ null
            $ concatMap
                (SharedSearch.batchCandidates . SharedQuery.resultSearch)
                boundaryResults
      , testCase "legacy validation preserves compound-error precedence" $ do
          let duplicateName = name "duplicate"
              binding = FunctionBinding (TypeVar 0) duplicateName 0 [] []
              polymorphic = TypeForall [1] [] $ TypeVar 1
              nestedGoal = TypeArrow polymorphic polymorphic
              invalidHeuristics = defaultHeuristicsConfig
                { heuristics_goalVar = -1
                , heuristics_goalCons = -2
                }
              compound = identityInput
                { input_goalType = nestedGoal
                , input_envFuncs = [binding, binding]
                , input_allowConstraintsStopStep = -1
                , input_maxSteps = 0
                , input_maxQueueSize = Just (-1)
                , input_maxDepth = Just (-1)
                , input_heuristicsConfig = invalidHeuristics
                }
          validateExferenceInput compound @?= Left (InvalidMaxSteps 0)
          validateExferenceInput compound {input_maxSteps = 20} @?=
            Left (InvalidConstraintDeferralSteps (-1))
          validateExferenceInput compound
              { input_maxSteps = 20
              , input_allowConstraintsStopStep = 0
              } @?= Left (InvalidMaxQueueSize (-1))
          validateExferenceInput compound
              { input_maxSteps = 20
              , input_allowConstraintsStopStep = 0
              , input_maxQueueSize = Nothing
              } @?= Left (InvalidMaxDepth (-1))
          validateExferenceInput compound
              { input_maxSteps = 20
              , input_allowConstraintsStopStep = 0
              , input_maxQueueSize = Nothing
              , input_maxDepth = Nothing
              } @?=
            Left (DuplicateFunctionNames [duplicateName])
          validateExferenceInput compound
              { input_envFuncs = [binding]
              , input_maxSteps = 20
              , input_allowConstraintsStopStep = 0
              , input_maxQueueSize = Nothing
              , input_maxDepth = Nothing
              } @?= Left (InvalidHeuristic "goalVar" (-1))
          validateExferenceInput compound
              { input_envFuncs = [binding]
              , input_maxSteps = 20
              , input_allowConstraintsStopStep = 0
              , input_maxQueueSize = Nothing
              , input_maxDepth = Nothing
              , input_heuristicsConfig = invalidHeuristics
                  {heuristics_goalVar = 0}
              } @?= Left (InvalidHeuristic "goalCons" (-2))
          validateExferenceInput compound
              { input_envFuncs = [binding]
              , input_maxSteps = 20
              , input_allowConstraintsStopStep = 0
              , input_maxQueueSize = Nothing
              , input_maxDepth = Nothing
              , input_heuristicsConfig = defaultHeuristicsConfig
              } @?= Right ()
      ]
  , testGroup "search policy"
      [ testCase "duplicate function names are complete and order-independent" $ do
          let firstName = name "f"
              secondName = name "g"
              binding result bindingName penalty = FunctionBinding
                (TypeCons $ name result) bindingName penalty [] []
              firstInt = binding "Int" firstName 0
              firstBool = binding "Bool" firstName 1
              secondInt = binding "Int" secondName 2
              secondBool = binding "Bool" secondName 3
              expected = Left
                $ DuplicateFunctionNames [firstName, secondName]
          validateExferenceInput identityInput
            { input_envFuncs =
                [secondInt, firstInt, secondBool, firstBool]
            } @?= expected
          validateExferenceInput identityInput
            { input_envFuncs =
                [firstBool, secondBool, firstInt, secondInt]
            } @?= expected
      , testCase "binding usage retains nominal identities" $ do
          firstName <- expectRight $ mkQualifiedName ["First"] "choose"
          secondName <- expectRight $ mkQualifiedName ["Second"] "choose"
          let intType = TypeCons $ name "Int"
              boolType = TypeCons $ name "Bool"
              choose bindingName = FunctionBinding
                { functionResult = boolType
                , functionName = bindingName
                , functionPenalty = 0
                , functionConstraints = []
                , functionParameters = [intType]
                }
              seed = FunctionBinding
                { functionResult = intType
                , functionName = name "seed"
                , functionPenalty = 0
                , functionConstraints = []
                , functionParameters = []
                }
          terminal <- lastChunk $ identityInput
            { input_goalType = boolType
            , input_envFuncs = [choose firstName, choose secondName, seed]
            , input_maxSteps = 100
            }
          let usages = chunkBindingUsages terminal
          assertBool "first qualified binding disappeared from usage metadata"
            $ Map.member firstName usages
          assertBool "second qualified binding disappeared from usage metadata"
            $ Map.member secondName usages
          assertBool "terminal binding application was not counted"
            $ Map.member (name "seed") usages
      , testCase "partial binding applications are counted when generated" $ do
          let bridgeName = name "bridge"
              bridge = FunctionBinding
                { functionResult = TypeCons $ name "Bool"
                , functionName = bridgeName
                , functionPenalty = 0
                , functionConstraints = []
                , functionParameters = [TypeCons $ name "Int"]
                }
          chunk <- lastChunk $ identityInput
            { input_goalType = TypeCons $ name "String"
            , input_envFuncs = [bridge]
            -- The first step introduces the outer forall wrapper; the second
            -- generates the speculative partial application.
            , input_maxSteps = 2
            }
          Map.lookup bridgeName (chunkBindingUsages chunk) @?= Just 1
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
      , testCase "headless deconstructors cannot manufacture pattern matches" $ do
          let datatype = TypeCons $ name "T"
              boolean = TypeCons $ name "Bool"
              bogus = DeconstructorBinding (TypeVar 0)
                [ConstructorBinding (name "Bogus") []] False
              input = identityInput
                { input_goalType = TypeArrow datatype boolean
                , input_envFuncs =
                    [FunctionBinding boolean (name "True") 0 [] []]
                , input_envDeconsS = [bogus]
                , input_maxSteps = 50
                }
              expected = DeconstructorInputWithoutNominalHead $ TypeVar 0
          validateExferenceInput input @?= Left expected
          case findExpressionsWithStatsEither input of
            Left actual -> actual @?= expected
            Right _ -> fail
              "a headless deconstructor reached pattern-match synthesis"
      , testCase "the function constructor is not a datatype head" $ do
          let arrowName = validQualifiedName [] "->"
              functionType = TypeApp
                (TypeApp (TypeCons arrowName) $ TypeVar 0)
                (TypeVar 1)
              deconstructor = DeconstructorBinding functionType
                [ConstructorBinding (name "Function") []] False
          validateExferenceInput identityInput
            { input_envDeconsS = [deconstructor] }
            @?= Left (UnsupportedDeconstructorTypeHead arrowName)
      , testCase "constructor fields cannot introduce datatype variables" $ do
          let boxName = name "Box"
              boxType = TypeApp (TypeCons boxName) $ TypeVar 0
              boxConstructor = name "MkBox"
              deconstructor = DeconstructorBinding boxType
                [ConstructorBinding boxConstructor [TypeVar 1]] False
              expected = UnboundDeconstructorFieldVariables
                boxName boxConstructor [1]
              environment = EnvDictionary [] [deconstructor] emptyClassEnv
          validateExferenceInput identityInput
            { input_envDeconsS = [deconstructor] } @?= Left expected
          case mkExferenceEnvironment environment of
            Left actual -> actual @?= expected
            Right _ -> fail
              "an unbound constructor-field variable reached a sealed environment"
      , testCase "parameterized and recursive deconstructors remain valid" $ do
          let boxName = name "Box"
              boxType = TypeApp (TypeCons boxName) $ TypeVar 0
              box = DeconstructorBinding boxType
                [ConstructorBinding (name "MkBox") [TypeVar 0]] False
              treeName = name "Tree"
              treeType = TypeApp (TypeCons treeName) $ TypeVar 1
              tree = DeconstructorBinding treeType
                [ ConstructorBinding (name "Leaf") [TypeVar 1]
                , ConstructorBinding (name "Branch") [treeType, treeType]
                ] True
              deconstructors = [box, tree]
              environment = EnvDictionary [] deconstructors emptyClassEnv
          validateExferenceInput identityInput
            { input_envDeconsS = deconstructors } @?= Right ()
          case mkExferenceEnvironment environment of
            Left failure -> fail
              $ "valid deconstructors failed environment sealing: " ++ show failure
            Right _ -> pure ()
      , testCase "generated constructor patterns are validated at input" $ do
          let arrowName = validQualifiedName [] "->"
              invalidBinding = FunctionBinding
                (TypeVar 0) arrowName 0 [] []
          validateExferenceInput identityInput
            { input_envFuncs = [invalidBinding] } @?= Left
              (InvalidGeneratedBinding arrowName
                $ Generated.InvalidGlobalExpression SharedName.functionName)

          let invalidName = name "notAConstructor"
              invalid = DeconstructorBinding
                (TypeCons $ name "T")
                [ConstructorBinding invalidName []]
                False
          validateExferenceInput identityInput
            { input_envDeconsS = [invalid] } @?= Left
              (InvalidGeneratedConstructor invalidName
                $ Generated.InvalidConstructorPattern
                invalidName)

          let malformedCons = DeconstructorBinding
                (TypeApp (TypeCons ListCon) $ TypeVar 0)
                [ConstructorBinding Cons [TypeVar 0]]
                False
          validateExferenceInput identityInput
            { input_envDeconsS = [malformedCons] } @?= Left
              (InvalidGeneratedConstructor Cons
                $ Generated.InvalidConstructorPatternArity
                    SharedName.consName 2 1)
      , testCase "constraint relaxation ends at the configured step" $ do
          constraintsRelaxedAtStep False 2 1 @?= True
          constraintsRelaxedAtStep False 2 2 @?= True
          constraintsRelaxedAtStep False 2 3 @?= False
          constraintsRelaxedAtStep True 2 3 @?= True
      , testCase "scheduler separates solutions from pending sibling work" $ do
          let integer = TypeCons $ name "Int"
              constantName = name "constant"
              wrapperName = name "wrapper"
              constant = FunctionBinding integer constantName 0 [] []
              wrapper = FunctionBinding integer wrapperName 0 [] [integer]
              input = identityInput
                { input_goalType = integer
                , input_envFuncs = [constant, wrapper]
                , input_maxSteps = 3
                }
          chunks <- expectRight $ findExpressionsWithStatsEither input
          case take 3 chunks of
            [opened, solved, continued] -> do
              -- forallify contributes the first pending root step.
              assertBool "opening the root unexpectedly solved it"
                $ null $ chunkElements opened
              assertBool "the immediate solution entered the pending queue"
                $ map (\(expression, _, _) -> expression)
                    (chunkElements solved)
                == [ExpName constantName]
              assertBool "pending wrapper work lost its extracted goal"
                $ map (\(expression, _, _) -> expression)
                    (chunkElements continued)
                == [ExpApply (ExpName wrapperName) (ExpName constantName)]
            actual -> fail $ "expected three scheduler chunks, got "
              ++ show (length actual)
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
      , testCase "nested forall goals remain opaque after root opening" $ do
          let polymorphic = TypeForall [0] [] (TypeVar 0)
              goal = TypeArrow polymorphic polymorphic
              input = ExferenceInput goal [] [] emptyClassEnv
                False False 0 False 20 Nothing Nothing defaultHeuristicsConfig
          validateExferenceInput input @?= Right ()
          case findExpressionsEither input of
            Left failure -> fail $ "rank-N goal was rejected: " ++ show failure
            Right expressions -> assertBool
              "rank-N identity did not use its opaque scoped argument"
              $ not $ null expressions
          case findOneExpression input of
            Just
                ( ExpLambda binder declared (ExpVar returned occurrence)
                , residual
                , _
                ) -> do
              returned @?= binder
              declared @?= polymorphic
              occurrence @?= polymorphic
              residual @?= []
            Nothing -> fail "exact opaque forwarding produced no expression"
            Just (expression, residual, _) -> fail
              $ "unexpected exact opaque forwarding result: "
              ++ showExpression expression
              ++ " with constraints " ++ show residual
          case findExpressionsWithStatsEither input of
            Left failure -> fail
              $ "checked rank-N chunk search failed: " ++ show failure
            Right _ -> pure ()
          case Core.findExpressionsChunkedEither input of
            Left failure -> fail
              $ "grouped rank-N search failed: " ++ show failure
            Right _ -> pure ()
          let nestedConstraint = HsConstraint (name "Inner") [TypeVar 1]
              outerConstraint = HsConstraint (name "Outer")
                [TypeForall [1] [nestedConstraint] $ TypeVar 1]
              constrainedGoal = TypeForall [0] [outerConstraint] $ TypeVar 0
          typeConstraints constrainedGoal
            @?= [outerConstraint, nestedConstraint]
          validateExferenceInput input { input_goalType = constrainedGoal }
            @?= Right ()
      , testCase "scoped provider foralls instantiate at monomorphic uses" $ do
          let integer = TypeCons $ name "Int"
              polymorphic = TypeForall [0] []
                $ TypeArrow (TypeVar 0) (TypeVar 0)
              goal = TypeArrow polymorphic $ TypeArrow integer integer
              input = identityInput
                { input_goalType = goal
                , input_maxSteps = 100
                }
          (expression, residual, _) <- maybe
            (fail "a polymorphic scoped identity was not instantiated") pure
            $ findOneExpression input
          residual @?= []
          case expression of
            -- Eta reduction is sound here because the occurrence annotation
            -- records the monomorphic instantiation independently of the
            -- lambda binder's declared scheme.
            ExpLambda binder declared (ExpVar returned instantiated) -> do
              returned @?= binder
              declared @?= polymorphic
              assertBool
                ("unexpected provider instantiation: "
                  ++ showHsType Map.empty instantiated)
                $ case unifyShared instantiated
                    (TypeArrow integer integer) of
                    Just _ -> True
                    Nothing -> False
            _ -> fail $ "unexpected provider elimination result: "
              ++ showExpression expression
          checkExpression (mkQueryClassEnv emptyClassEnv []) [] []
            goal [] expression @?= Right ()
      , testCase "bare forall providers instantiate flexible occurrences" $ do
          let unit = TypeTuple Boxed []
              vacuousUnit = TypeForall [] [] unit
              polymorphic = TypeForall [0] [] $ TypeVar 0
              goal = TypeArrow polymorphic unit
              input = identityInput
                { input_goalType = goal
                , input_maxSteps = 100
                }
          (expression, residual, _) <- maybe
            (fail "forall a. a was not instantiated at unit") pure
            $ findOneExpression input
          residual @?= []
          checkExpression (mkQueryClassEnv emptyClassEnv []) [] []
            goal [] expression @?= Right ()
          (wrappedExpression, wrappedResidual, _) <- maybe
            (fail "a vacuous forall wrapper suppressed provider instantiation")
            pure
            $ findOneExpression
            $ input {input_goalType = TypeArrow polymorphic vacuousUnit}
          wrappedResidual @?= []
          checkExpression (mkQueryClassEnv emptyClassEnv []) [] []
            (TypeArrow polymorphic vacuousUnit) [] wrappedExpression @?= Right ()
          let distinctPolymorphic = TypeForall [1] []
                $ TypeArrow (TypeVar 1) (TypeVar 1)
              quantifiedGoal =
                TypeArrow polymorphic distinctPolymorphic
          (quantifiedExpression, quantifiedResidual, _) <- maybe
            (fail "a more-general provider did not subsume a quantified goal")
            pure
            $ findOneExpression $ input {input_goalType = quantifiedGoal}
          quantifiedResidual @?= []
          case quantifiedExpression of
            ExpLambda binder declared (ExpVar returned annotation) -> do
              returned @?= binder
              declared @?= polymorphic
              annotation @?= distinctPolymorphic
            unexpected -> fail $ "unexpected subsumed provider expression: "
              ++ showExpression unexpected
          checkExpression (mkQueryClassEnv emptyClassEnv []) [] []
            quantifiedGoal [] quantifiedExpression @?= Right ()
      , testCase "provider forall contexts become proof obligations" $ do
          let className = name "C"
              integer = TypeCons $ name "Int"
              evidence variable = HsConstraint className [variable]
              polymorphic = TypeForall [0] [evidence $ TypeVar 0]
                $ TypeArrow (TypeVar 0) (TypeVar 0)
              goal = TypeArrow polymorphic $ TypeArrow integer integer
          withoutInstance <- expectRight
            $ mkStaticClassEnv [HsTypeClass className [0] []] []
          withInstance <- expectRight
            $ mkStaticClassEnv
                [HsTypeClass className [0] []]
                [HsInstance [] $ evidence integer]
          let input environment = identityInput
                { input_goalType = goal
                , input_envClasses = environment
                , input_maxSteps = 100
                }
              exactInput = (input withoutInstance)
                {input_goalType = TypeArrow polymorphic polymorphic}
          case findOneExpression exactInput of
            Just (_, exactResidual, _) -> exactResidual @?= []
            Nothing -> fail
              "exact polymorphic forwarding incorrectly required C evidence"
          case findOneExpression $ input withoutInstance of
            Nothing -> pure ()
            Just (unexpected, _, _) -> fail
              $ "an unresolved provider context produced "
              ++ showExpression unexpected
          (_, residual, _) <- maybe
            (fail "residual provider context was discarded") pure
            $ findOneExpression
            $ (input withoutInstance) {input_allowConstraints = True}
          residual @?= [evidence integer]
          (expression, solved, _) <- maybe
            (fail "a matching instance did not solve the provider context") pure
            $ findOneExpression $ input withInstance
          solved @?= []
          checkExpression (mkQueryClassEnv withInstance []) [] []
            goal [] expression @?= Right ()
      , testCase "rank-N fields instantiate after pattern elimination" $ do
          let integer = TypeCons $ name "Int"
              boxType = TypeCons $ name "PolyBox"
              constructor = name "PolyBox"
              polymorphic = TypeForall [0] []
                $ TypeArrow (TypeVar 0) (TypeVar 0)
              deconstructor = DeconstructorBinding boxType
                [ConstructorBinding constructor [polymorphic]] False
              goal = TypeArrow boxType $ TypeArrow integer integer
              input = identityInput
                { input_goalType = goal
                , input_envDeconsS = [deconstructor]
                , input_maxSteps = 100
                }
          (expression, residual, _) <- maybe
            (fail "a rank-N field could not be selected and instantiated") pure
            $ findOneExpression input
          residual @?= []
          checkExpression (mkQueryClassEnv emptyClassEnv []) []
            [deconstructor] goal [] expression @?= Right ()
      , testCase "generic constructors instantiate impredicatively" $ do
          let polymorphic = TypeForall [0] []
                $ TypeArrow (TypeVar 0) (TypeVar 0)
              listOf element = TypeApp
                (TypeCons SharedName.listName) element
              emptyName = name "emptyPolyList"
              emptyList = FunctionBinding
                (listOf $ TypeVar 1) emptyName 0 [] []
              goal = listOf polymorphic
              input = identityInput
                { input_goalType = goal
                , input_envFuncs = [emptyList]
                }
          (expression, constraints, _) <- maybe
            (fail "generic empty list did not instantiate to a polytype") pure
            $ findOneExpression input
          assertBool ("unexpected impredicative constructor result: "
              ++ showExpression expression)
            $ expression == ExpName emptyName
          constraints @?= []
          checkExpression (mkQueryClassEnv emptyClassEnv []) [emptyList] []
            goal [] expression @?= Right ()
      , testCase "input errors render native types as Haskell" $ do
          let polymorphic = TypeForall [0] []
                $ TypeArrow (TypeVar 0) (TypeVar 0)
              goalFailure = show $ NestedForallInGoal polymorphic
              constraint = HsConstraint (name "C") [TypeVar 0]
              classFailure = show $ InvalidClassConstraint
                $ DuplicateInstanceHeads [constraint]
          assertBool ("structural goal leaked into diagnostic: " ++ goalFailure)
            $ not ("ForallType" `isInfixOf` goalFailure)
              && not ("TypeVariable" `isInfixOf` goalFailure)
              && "forall" `isInfixOf` goalFailure
          assertBool
            ("structural constraint leaked into diagnostic: " ++ classFailure)
            $ not ("TypeVariable" `isInfixOf` classFailure)
              && "C v0" `isInfixOf` classFailure
      , testCase "rank-N atoms are accepted throughout the environment" $ do
          let polymorphic = TypeForall [1] [] $ TypeVar 1
              bindingName = name "f"
              bindingConstraint = HsConstraint (name "External")
                [polymorphic]
              constrainedBinding = FunctionBinding
                (TypeVar 0) bindingName 0 [bindingConstraint] []
          validateExferenceInput identityInput
            { input_envFuncs = [constrainedBinding] }
            @?= Right ()

          let polymorphicBinding = FunctionBinding
                polymorphic bindingName 0 [] []
          validateExferenceInput identityInput
            { input_envFuncs = [polymorphicBinding] }
            @?= Right ()

          let deconstructor = DeconstructorBinding
                (TypeCons $ name "Box")
                [ConstructorBinding (name "Box") [polymorphic]] False
          validateExferenceInput identityInput
            { input_envDeconsS = [deconstructor] }
            @?= Right ()
      , testCase "shared type validation covers otherwise-valid prenex input" $ do
          let duplicate = TypeForall [0, 0] [] $ TypeVar 0
          validateExferenceInput identityInput
            { input_goalType = duplicate }
            @?= Left (InvalidInputType duplicate
              $ InvalidSynthesisType
              $ SharedType.DuplicateForallVariable
              $ SharedType.FlexibleVariable 0)
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
      , testCase "candidate statistics count completed search steps" $ do
          chunk <- lastChunk identityInput
          let candidateSteps =
                [ exference_steps statistics
                | (_, _, statistics) <- chunkElements chunk
                ]
          candidateSteps @?= [3]
      , testCase "case scrutinees count as one use regardless of constructor count" $ do
          let bool = TypeCons $ name "Bool"
              seed = TypeCons $ name "Seed"
              result = TypeCons $ name "Result"
              viaCaseName = name "viaCase"
              viaSeedName = name "viaSeed"
              trueName = name "true"
              functions =
                [ FunctionBinding result viaCaseName (-2) []
                    [TypeArrow bool bool]
                , FunctionBinding result viaSeedName 4 [] [seed]
                , FunctionBinding bool trueName 0 [] []
                ]
              boolDeconstructor = DeconstructorBinding bool
                [ ConstructorBinding (name "False") []
                , ConstructorBinding (name "True") []
                ] False
              runWith penalty = lastChunk $ identityInput
                { input_goalType = result
                , input_envFuncs = functions
                , input_envDeconsS = [boolDeconstructor]
                , input_multiPM = True
                , input_maxSteps = 4
                , input_heuristicsConfig = defaultHeuristicsConfig
                    {heuristics_tempMultiVarUsePenalty = penalty}
                }
          baseline <- runWith 0
          heavilyWeighted <- runWith 100
          let baselineUses = Map.lookup trueName
                $ chunkBindingUsages baseline
              weightedUses = Map.lookup trueName
                $ chunkBindingUsages heavilyWeighted
          -- After the initial forall opening, the case branch competes with
          -- the Seed branch at step four.
          -- Reaching `true` calibrates that the case branch won; changing the
          -- multiple-use penalty must not demote it merely because Bool has
          -- two constructors.
          baselineUses @?= Just 1
          weightedUses @?= baselineUses
      , testCase "empty datatypes eliminate through search, checking, and rendering" $ do
          let emptyName = name "Empty"
              genericEmpty = TypeApp (TypeCons emptyName) $ TypeVar 0
              integer = TypeCons $ name "Int"
              integerEmpty = TypeApp (TypeCons emptyName) integer
              deconstructor = DeconstructorBinding genericEmpty [] False
              goal = TypeArrow integerEmpty integer
              input = identityInput
                { input_goalType = goal
                , input_envDeconsS = [deconstructor]
                -- Keep the strict default: finding this proof also verifies
                -- that the empty-case scrutinee was recorded as used.
                }
          (expression, constraints, _) <- maybe
            (fail "empty datatype elimination produced no expression") pure
            $ findOneExpression input
          constraints @?= []
          scrutinee <- case expression of
            ExpLambda variable annotation
                (ExpCaseMatch (ExpVar matched matchedAnnotation) []) -> do
              annotation @?= integerEmpty
              matched @?= variable
              matchedAnnotation @?= integerEmpty
              pure variable
            _ -> fail $ "unexpected empty elimination: "
              ++ showExpression expression
          checkExpression (mkQueryClassEnv emptyClassEnv []) []
            [deconstructor] goal [] expression @?= Right ()
          let binder = preferredVarName scrutinee integerEmpty
          renderExpression Generated.Unqualified expression @?= Right
            ("\\" ++ binder ++ " -> case " ++ binder ++ " of {}")
      , testCase "single-constructor patterns retain substituted field types" $ do
          let integer = TypeCons $ name "Int"
              boxName = name "Box"
              genericBox = TypeApp (TypeCons boxName) $ TypeVar 0
              integerBox = TypeApp (TypeCons boxName) integer
              deconstructor = DeconstructorBinding genericBox
                [ConstructorBinding boxName [TypeVar 0]] False
              input = identityInput
                { input_goalType = TypeArrow integerBox integer
                , input_envDeconsS = [deconstructor]
                }
          case findOneExpression input of
            Just
                ( ExpLambda scrutinee _
                    (ExpLetMatch constructor [(field, annotation)]
                      (ExpVar matchedScrutinee _)
                      (ExpVar returnedField _))
                , []
                , _
                ) -> do
              constructor @?= boxName
              matchedScrutinee @?= scrutinee
              returnedField @?= field
              annotation @?= integer
            Nothing -> fail "Box deconstruction produced no expression"
            Just (expression, constraints, _) -> fail
              $ "unexpected Box deconstruction result: "
              ++ showExpression expression
              ++ " with constraints " ++ show constraints
      , testCase "provided values consume supplied class evidence" $ do
          let className = name "Evidence"
              variable = TypeVar 0
              assumption = HsConstraint className [variable]
              goal = TypeForall [0] [assumption]
                $ TypeArrow variable variable
          staticClasses <- expectRight
            $ mkStaticClassEnv [HsTypeClass className [0] []] []
          let input = identityInput
                { input_goalType = goal
                , input_envClasses = staticClasses
                }
          case findOneExpression input of
            Just (ExpLambda binder _ (ExpVar returned _), residual, _) -> do
              returned @?= binder
              residual @?= []
            Nothing -> fail "supplied Evidence dictionary rejected identity"
            Just (expression, residual, _) -> fail
              $ "unexpected evidence-consuming result: "
              ++ showExpression expression
              ++ " with constraints " ++ show residual
      , testCase "complete failure is distinguished from bounded search" $ do
          let input = identityInput
                {input_goalType = TypeCons $ name "Void"}
          chunk <- lastChunk input
          assertBool "an uninhabited atomic goal produced an expression"
            $ null $ chunkElements chunk
          chunkStatus chunk @?= SearchStatus SearchExhausted 0 0
          target <- checkedIdentifierTarget "completeFailure"
          environment <- expectRight $ sealLegacyEnvironment input
          results <- expectRight $ findQueryResultsInEnvironmentEither
            target
            (emptyExferenceSourceTypeVariableHints $ input_goalType input)
            environment
            (legacyInputQuery input)
          result <- case results of
            [] -> fail "complete canonical search produced no result batch"
            firstResult : remaining ->
              pure $ lastElement firstResult remaining
          let batch = SharedQuery.resultSearch result
          SharedQuery.resultEvidence result @?= SharedQuery.NoEvidence
          SharedSearch.batchProgress batch @?=
            SharedSearch.Completed SharedSearch.Finished
          assertBool "common result unexpectedly gained candidates"
            $ null $ SharedSearch.batchCandidates batch
      , testCase "partial-application lets retain the remaining arrow type" $ do
          -- Regression: the search annotated a partially applied let binding
          -- with the applier's final result type instead of the remaining
          -- arrow type, so the independent checker rejected -- and the search
          -- silently discarded -- every solution reusing the shared partial
          -- application, such as @let v = f a in h (v b) (v b)@ below.
          let atom spelling = TypeCons $ name spelling
              partialInput = identityInput
                { input_goalType = atom "C"
                , input_envFuncs =
                    [ FunctionBinding (atom "I") (name "f") 0 []
                        [atom "A", atom "B"]
                    , FunctionBinding (atom "C") (name "h") 0 []
                        [atom "I", atom "I"]
                    , FunctionBinding (atom "A") (name "a") 0 [] []
                    , FunctionBinding (atom "B") (name "b") 0 [] []
                    ]
                , input_maxSteps = 16384
                }
              isArrowType (TypeArrow _ _) = True
              isArrowType _ = False
              sharesPartialApplication expression = any (isArrowType . snd)
                $ expressionTypedLocals expression
              solutions =
                [ expression
                | chunk <- findExpressionsWithStats partialInput
                , (expression, _, _) <- chunkElements chunk
                ]
          assertBool "no emitted solution shared a partially applied binding"
            $ any sharesPartialApplication solutions
      , testCase "recursive scoped unification does not consume the step budget" $ do
          let variable = TypeVar 0
              applied constructor argument =
                TypeApp (TypeCons $ name constructor) argument
              provider = TypeArrow
                (applied "G" variable)
                (applied "H" $ applied "F" variable)
              scopedGoal = applied "H" variable
              result = TypeCons $ name "R"
              outer = FunctionBinding result (name "outer") 0 []
                [TypeArrow provider scopedGoal]
              heuristics = defaultHeuristicsConfig
                { heuristics_stepProvidedGood = 0
                , heuristics_stepProvidedBad = 1
                , heuristics_stepEnvGood = 0
                , heuristics_stepEnvBad = 1
                }
              input = identityInput
                { input_goalType = result
                , input_envFuncs = [outer]
                , input_maxSteps = 4
                , input_maxDepth = Just 0
                , input_heuristicsConfig = heuristics
                }
          chunk <- lastChunk input
          assertBool "cyclic scoped application produced a candidate"
            $ null $ chunkElements chunk
          -- Both legal partial applications exceed the depth cap.  Treating
          -- the scoped provider as an independent namespace would instead
          -- misclassify @H a ~ H (F a)@ as a cheap direct match and leave a
          -- checker-doomed node queued at the step limit.
          chunkStatus chunk @?= SearchStatus SearchPruned 0 2
      , testCase "queue pruning is bounded and reported" $ do
          chunk <- onlyChunk $ identityInput {input_maxQueueSize = Just 0}
          searchCompletion (chunkStatus chunk) @?= SearchPruned
          searchQueuePruned (chunkStatus chunk) @?= 1
      , testCase "depth pruning is configured and reported" $ do
          let config = defaultHeuristicsConfig
                {heuristics_functionGoalTransform = 1}
          chunk <- onlyChunk $ identityInput
            { input_maxDepth = Just 0
            , input_heuristicsConfig = config
            }
          searchCompletion (chunkStatus chunk) @?= SearchPruned
          searchDepthPruned (chunkStatus chunk) @?= 1
      , testCase "structural tuple searches match application spellings" $ do
          tupleConstructor <- expectRight $ SharedName.tupleName Boxed 3
          let elements =
                [ TypeVar 0
                , TypeConstant 1
                , TypeArrow (TypeVar 2) (TypeConstant 3)
                ]
              structural = TypeTuple Boxed elements
              legacy = foldl TypeApp (TypeCons tupleConstructor) elements
              fractional = defaultHeuristicsConfig
                { heuristics_goalVar = 0.1
                , heuristics_goalCons = 0.2
                , heuristics_goalArrow = 0.3
                , heuristics_goalApp = 0.4
                }
              near divisor = Penalty
                $ Score.penaltyValue Score.maxPenalty / divisor
              nearSaturation = defaultHeuristicsConfig
                { heuristics_goalVar = near 13
                , heuristics_goalCons = near 11
                , heuristics_goalArrow = near 9
                , heuristics_goalApp = near 7
                }
              searchTrace config representation = do
                chunks <- expectRight $ findExpressionsWithStatsEither
                  identityInput
                    { input_goalType = TypeArrow representation representation
                    , input_heuristicsConfig = config
                    }
                pure
                  [ (chunkStatus chunk, chunkBindingUsages chunk,
                      chunkElements chunk)
                  | chunk <- chunks
                  ]
              assertEquivalent label config = do
                applicationTrace <- searchTrace config legacy
                structuralTrace <- searchTrace config structural
                assertBool label $ applicationTrace == structuralTrace
          assertEquivalent "fractional public trace" fractional
          assertEquivalent "near-saturation public trace" nearSaturation
      , testCase "solution length contributes structural candidate cost" $ do
          let firstStatistics input = take 1
                [ statistics
                | chunk <- findExpressionsWithStats input
                , (_, _, statistics) <- chunkElements chunk
                ]
              withoutLength = defaultHeuristicsConfig
                {heuristics_solutionLength = 0}
              withLength = defaultHeuristicsConfig
                {heuristics_solutionLength = 1}
          case ( firstStatistics identityInput
                    {input_heuristicsConfig = withoutLength}
               , firstStatistics identityInput
                    {input_heuristicsConfig = withLength}
               ) of
            ([baseline], [weighted]) ->
              exference_complexityRating weighted @?=
                exference_complexityRating baseline + 2
            result -> fail $ "expected two identity candidates, got "
              ++ show result
      , testCase "negative constraint-deferral steps are rejected" $ do
          let invalid = identityInput
                { input_allowConstraintsStopStep = -1 }
              expected = InvalidConstraintDeferralSteps (-1)
              assertFailure description result = case result of
                Left actual -> actual @?= expected
                Right _ -> fail $ description ++ " discarded validation failure"
          validateExferenceInput invalid @?= Left expected
          assertFailure "the checked flat API" $ findExpressionsEither invalid
          assertFailure "the checked grouped API"
            $ Core.findExpressionsChunkedEither invalid
          assertFailure "the checked status API"
            $ findExpressionsWithStatsEither invalid
      , testCase "non-finite heuristic inputs are rejected" $ do
          let config = defaultHeuristicsConfig
                {heuristics_goalVar = Penalty (0 / 0)}
          case findExpressionsEither identityInput
              {input_heuristicsConfig = config} of
            Left (InvalidHeuristic "goalVar" (Penalty value)) ->
              isNaN value @?= True
            Left other -> fail $ "unexpected validation error: " ++ show other
            Right _ -> fail "non-finite heuristic was accepted"
      , testCase "finite score operations saturate without changing ordinary order" $ do
          let maximumFinite = 1.7976931348623157e308
              maximumScore = Penalty maximumFinite
              minimumScore = Penalty $ negate maximumFinite
              nanPenalty = Penalty $ 0 / 0
              nanPriority = Score.Priority $ 0 / 0
              saturatedUpper = Score.addScore maximumScore maximumScore
              saturatedLower = Score.addScore minimumScore minimumScore
              saturatedPriority = Score.addPriority
                (Score.Priority maximumFinite) (Score.Priority maximumFinite)
          Score.addScore (Penalty 1.25) (Penalty 2.5)
            @?= Penalty 3.75
          Score.addScore (Penalty (-3.5)) (Penalty 1)
            @?= Penalty (-2.5)
          compare (Penalty (-3.5)) (Penalty 2) @?= LT
          compare (Score.Priority (-3.5)) (Score.Priority 2) @?= LT
          penaltyValue saturatedUpper @?= maximumFinite
          penaltyValue saturatedLower @?= negate maximumFinite
          Score.priorityValue saturatedPriority @?= maximumFinite
          assertBool "saturating score addition produced a non-finite value"
            $ Score.isFiniteScore saturatedUpper
              && Score.isFiniteScore saturatedLower
          assertBool "saturating priority addition produced a non-finite key"
            $ Score.isFinitePriority saturatedPriority
          -- Public numeric expressions stay raw until validation rather than
          -- silently turning a malformed option into an accepted maximum.
          case (0 / 0 :: Penalty) of
            Penalty value -> assertBool "raw score arithmetic hid NaN"
              $ isNaN value
          nanPenalty == nanPenalty @?= True
          compare nanPenalty nanPenalty @?= EQ
          nanPriority == nanPriority @?= True
          compare nanPriority nanPriority @?= EQ
          let normalizedPriority = Score.normalizePriority nanPriority
          assertBool "normalizing a queue NaN did not produce a finite key"
            $ Score.isFinitePriority normalizedPriority
          assertBool "a normalized queue NaN did not sink"
            $ normalizedPriority < 0
      , testCase "maximum finite depth costs saturate candidate metrics" $ do
          let maximumFinite = 1.7976931348623157e308
              input = identityInput
                { input_heuristicsConfig = defaultHeuristicsConfig
                    { heuristics_functionGoalTransform =
                        Penalty maximumFinite
                    }
                }
          validateExferenceInput input @?= Right ()
          chunks <- expectRight $ findExpressionsWithStatsEither input
          case [ exference_complexityRating statistics
               | chunk <- chunks
               , (_, _, statistics) <- chunkElements chunk
               ] of
            rating : _ -> do
              Score.isFiniteScore rating @?= True
              penaltyValue rating @?= maximumFinite
            [] -> fail "extreme finite identity search produced no candidate"
      , testCase "opposing extreme finite scores cannot produce NaN" $ do
          let maximumFinite = 1.7976931348623157e308
              argument = TypeCons $ name "Argument"
              result = TypeCons $ name "Result"
              preferred = Penalty $ negate maximumFinite
              outer = FunctionBinding result (name "outer") preferred
                [] [argument]
              inner = FunctionBinding argument (name "inner") preferred
                [] []
              input = identityInput
                { input_goalType = result
                , input_envFuncs = [outer, inner]
                , input_heuristicsConfig = defaultHeuristicsConfig
                    {heuristics_solutionLength = Penalty maximumFinite}
                }
          validateExferenceInput input @?= Right ()
          chunks <- expectRight $ findExpressionsWithStatsEither input
          case [ exference_complexityRating statistics
               | chunk <- chunks
               , (_, _, statistics) <- chunkElements chunk
               ] of
            rating : _ -> assertBool
              ("extreme finite inputs produced " ++ show rating)
              $ Score.isFiniteScore rating
            [] -> fail "extreme signed-rating search produced no candidate"
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
      [ testCase "legacy compatibility patterns remain match-compatible" $ do
          ordinary <- expectRight $ mkQualifiedName [] "id"
          unit <- expectRight $ mkBoxedTupleName 0
          map CompatibilityImport.legacyConstructorView
            [ordinary, ListCon, unit, Cons]
            @?= ["ordinary:id", "list", "tuple:0", "cons"]
      , testCase "checked ordinary construction canonicalizes operators" $ do
          operator <- expectRight
            $ mkQualifiedName ["Control", "Applicative"] "(<*>)"
          show operator @?= "Control.Applicative.(<*>)"
          qualifiedNameOperator operator @?= Just "<*>"
          function <- expectRight $ mkQualifiedName [] "->"
          qualifiedNameOperator function @?= Just "->"
          function @?= validQualifiedName [] "->"
      , testCase "checked ordinary construction rejects contextual syntax" $
          mapM_ (assertNameRejected . uncurry mkQualifiedName)
            [ ([], " map")
            , ([], "map ")
            , ([], "`map`")
            , (["Data"], "Data.map")
            ]
      , testCase "native alias admits the complete shared name domain" $ do
          shared <- expectRight $ SharedName.tupleName SharedName.Unboxed 2
          CompatibilityImport.legacyConstructorView shared @?=
            "unboxed-tuple:2"
      , testCase "Eq, Ord, Show, and NFData observe one value" $ do
          value <- expectRight $ mkQualifiedName ["Data", "List"] "map"
          value @?= value
          compare value value @?= EQ
          show value @?= "Data.List.map"
          force value @?= value
      , testCase "checked name builders reject invalid source values" $ do
          mapM_ (assertNameRejected . uncurry mkQualifiedName)
            [ ([], "")
            , ([], "case")
            , (["bad"], "x")
            ]
          mapM_ (assertNameRejected . mkBoxedTupleName) [-1, 1]
      , testCase "constraint access is structural and conversion stays total" $ do
          className <- expectRight $ mkQualifiedName ["Fixture"] "Class"
          let arguments = [TypeVar 2, TypeCons ListCon]
              constraint = HsConstraint className arguments
          CompatibilityImport.legacyConstraintClass
            (CompatibilityImport.legacyConstraint className)
            @?= className
          constraint_tclass constraint @?= className
          constraint_params constraint @?= arguments
          shared <- expectRight $ toSynthesisConstraint constraint
          fromSynthesisConstraint shared @?= Right constraint
          environment <- expectRight $ mkStaticClassEnv
            [HsTypeClass className [0, 1] []] []
          validateExferenceInput identityInput
            { input_goalType = TypeForall [2] [constraint] (TypeVar 2)
            , input_envClasses = environment
            }
            @?= Right ()
      ]
  , testGroup "parsing and diagnostics"
      [ testCase "caught conversions retain failed-branch allocations" $ do
          let syntaxName spelling = HSE.Ident HSE.noSrcSpan spelling
              action :: ConversionT String Identity Int
              action = catchE
                (getVar (syntaxName "failed") >> throwE "expected failure")
                (const $ getVar $ syntaxName "recovered")
              result = runIdentity $ runExceptT
                $ runConversionTWithState emptyConvData action
          case result of
            Right (recoveredId, finalState) -> do
              recoveredId @?= 1
              convDataTypeVarIndex finalState @?=
                Map.fromList [("failed", 0), ("recovered", 1)]
              convDataReservedIds finalState @?= IntSet.fromList [0, 1]
            Left failure -> fail $ "conversion did not recover: " ++ failure
      , testCase "sparse conversion hints cannot collide with allocation" $ do
          let syntaxName spelling = HSE.Ident HSE.noSrcSpan spelling
              initialHints = Map.fromList [("zero", 0), ("two", 2)]
              result = runIdentity $ runExceptT
                $ runConversionTWithState
                    (convDataFromTypeVarIndex initialHints)
                $ getVar $ syntaxName "fresh"
          case result of
            Right (freshId, finalState) -> do
              freshId @?= 3
              convDataTypeVarIndex finalState @?=
                Map.insert "fresh" 3 initialHints
              convDataReservedIds finalState @?= IntSet.fromList [0, 2, 3]
            Left failure -> fail $ "sparse allocation failed: " ++ failure
      , testCase "conversion hints preserve aliases for one ID" $ do
          let syntaxName spelling = HSE.Ident HSE.noSrcSpan spelling
              aliases = Map.fromList [("alpha", 7), ("beta", 7)]
              result = runIdentity $ runExceptT
                $ runConversionTWithState
                    (convDataFromTypeVarIndex aliases)
                $ getVar $ syntaxName "gamma"
          showHsType aliases (TypeVar 7) @?= "alpha"
          case result of
            Right (freshId, finalState) -> do
              freshId @?= 8
              convDataTypeVarIndex finalState @?=
                Map.insert "gamma" 8 aliases
              convDataReservedIds finalState @?= IntSet.fromList [7, 8]
            Left failure -> fail $ "alias-preserving allocation failed: "
              ++ failure
      , testCase "conversion allocation finds gaps below maxBound" $ do
          let syntaxName spelling = HSE.Ident HSE.noSrcSpan spelling
              initialHints = Map.singleton "top" maxBound
              action :: ConversionT String Identity (Int, Int)
              action = (,)
                <$> getVar (syntaxName "first")
                <*> getVar (syntaxName "second")
              result = runIdentity $ runExceptT
                $ runConversionTWithState
                    (convDataFromTypeVarIndex initialHints) action
          case result of
            Right ((firstId, secondId), finalState) -> do
              (firstId, secondId) @?= (0, 1)
              convDataTypeVarIndex finalState @?= Map.fromList
                [("first", 0), ("second", 1), ("top", maxBound)]
              convDataReservedIds finalState @?=
                IntSet.fromList [0, 1, maxBound]
            Left failure -> fail $ "boundary allocation failed: " ++ failure
      , testCase "hintless maxBound binders remain reserved" $ do
          let syntaxName spelling = HSE.Ident HSE.noSrcSpan spelling
              quantified = TypeForall [maxBound] [] $ TypeVar maxBound
              action :: ConversionT String Identity (HsType, Int)
              action = do
                normalized <- normalizeConvertedForalls
                  IntSet.empty quantified
                fresh <- getVar $ syntaxName "fresh"
                pure (normalized, fresh)
              result = runIdentity $ runExceptT
                $ runConversionTWithState emptyConvData action
          case result of
            Right ((normalized, freshId), finalState) -> do
              normalized @?= quantified
              freshId @?= 0
              convDataTypeVarIndex finalState @?= Map.singleton "fresh" 0
              convDataReservedIds finalState @?=
                IntSet.fromList [0, maxBound]
            Left failure -> fail $ "hidden boundary reservation failed: "
              ++ failure
      , testCase "caught failures cannot reuse an alpha-normalized ID" $ do
          let syntaxName spelling = HSE.Ident HSE.noSrcSpan spelling
              binder = syntaxName "a"
              quantified = HSE.TyForall HSE.noSrcSpan
                (Just [HSE.UnkindedVar HSE.noSrcSpan binder])
                Nothing
                (HSE.TyVar HSE.noSrcSpan binder)
              action :: ConversionT String Identity (HsType, Int)
              action = do
                normalized <- convertTypeNoDeclInternal
                  Map.empty Nothing [] quantified
                _ <- catchE
                  (throwE "expected failure")
                  (const $ pure ())
                recovered <- getVar $ syntaxName "recovered"
                pure (normalized, recovered)
              initial = convDataFromTypeVarIndex $ Map.singleton "a" 0
              result = runIdentity $ runExceptT
                $ runConversionTWithState initial action
          case result of
            Right ((normalized, recoveredId), finalState) -> do
              normalized @?= TypeForall [1] [] (TypeVar 1)
              recoveredId @?= 2
              convDataTypeVarIndex finalState @?=
                Map.fromList [("a", 0), ("recovered", 2)]
              convDataReservedIds finalState @?= IntSet.fromList [0, 1, 2]
            Left failure -> fail $ "conversion did not recover: " ++ failure
      , testCase "duplicate explicit forall binders are rejected" $ do
          let binder = HSE.Ident HSE.noSrcSpan "a"
              quantified = HSE.TyForall HSE.noSrcSpan
                (Just
                  [ HSE.UnkindedVar HSE.noSrcSpan binder
                  , HSE.UnkindedVar HSE.noSrcSpan binder
                  ])
                Nothing
                (HSE.TyVar HSE.noSrcSpan binder)
              result = runIdentity $ runExceptT
                $ convertTypeNoDecl Map.empty Nothing [] quantified
          result @?= Left "duplicate explicitly quantified type variable 0"
      , testCase "HSE tuple conversion bounds unchecked field spines" $ do
          let oversizedElements =
                replicate (SharedName.maximumTupleArity + 1)
                  (error "forced an invalid tuple element")
                ++ error "forced the oversized tuple tail"
              boxed = HSE.TyTuple HSE.noSrcSpan HSE.Boxed oversizedElements
              unboxed = HSE.TyTuple HSE.noSrcSpan HSE.Unboxed
                $ error "forced unsupported unboxed tuple fields"
              convert syntax = runIdentity $ runExceptT
                $ convertTypeNoDecl Map.empty Nothing [] syntax
          convert boxed @?= Left
            ("invalid boxed tuple arity "
              ++ show (SharedName.maximumTupleArity + 1))
          convert unboxed @?= Left "unsupported unboxed tuple type"
      , testCase "failed conversion runners hide their final state" $ do
          let action :: ConversionT String Identity ()
              action = do
                _ <- getVar $ HSE.Ident HSE.noSrcSpan "allocated"
                throwE "expected failure"
              result = runIdentity $ runExceptT
                $ runConversionTWithState emptyConvData action
          case result of
            Left failure -> failure @?= "expected failure"
            Right _ -> fail "failed conversion exposed a successful final state"
      , testCase "HSE name conversion is total for hand-built syntax" $ do
          let malformedOccurrence = HSE.Ident HSE.noSrcSpan ""
              malformedModule = HSE.ModuleName HSE.noSrcSpan "Data..Broken"
              assertConversionFailure label conversion = case conversion of
                Left _ -> pure ()
                Right value -> fail $ label ++ " accepted " ++ show value
          assertConversionFailure "empty occurrence"
            $ convertName malformedOccurrence
          assertConversionFailure "malformed module"
            $ convertModuleName malformedModule
                (HSE.Ident HSE.noSrcSpan "value")
      , testCase "ratings reject a missing value" $
          first (\failure ->
              (diagnosticSeverity failure, diagnosticMessage failure))
            (parseRatings "foo") @?= Left
              (Error, "rating file ends with a name but no numeric rating")
      , testCase "ratings reject a malformed number" $
          first (\failure ->
              (diagnosticSeverity failure, diagnosticMessage failure))
            (parseRatings "foo nope") @?= Left
              (Error, "invalid rating for foo: nope")
      , testCase "ratings reject non-finite values" $
          first (\failure ->
              (diagnosticSeverity failure, diagnosticMessage failure))
            (parseRatings "foo NaN") @?= Left
              (Error, "rating for foo must be finite: NaN")
      , testCase "source loading rejects duplicate explicit modules" $ do
          let firstPath = "/virtual/FirstA.hs"
              secondPath = "/virtual/SecondA.hs"
              thirdPath = "/virtual/ThirdA.hs"
              sources =
                [ (firstPath, "module A where\n")
                , (secondPath, "module A where\n")
                , (thirdPath, "module A where\n")
                ]
          LoadReport result diagnostics <- parseModuleSources sources
          diagnostics @?= []
          case result of
            Left (DuplicateModuleDeclarations failures@(failure :| _)) -> do
              let rendered = NonEmpty.toList failures
              map diagnosticSource rendered @?=
                [Just secondPath, Just thirdPath]
              map diagnosticContext rendered @?=
                [ [ "A is declared by both " ++ firstPath
                      ++ " and " ++ secondPath
                  ]
                , [ "A is declared by both " ++ firstPath
                      ++ " and " ++ thirdPath
                  ]
                ]
              diagnosticSeverity failure @?= Error
              diagnosticCode failure @?= Just "EXF_MODULE_DUPLICATE"
              fmap (sourceLine . sourceStart) (diagnosticSpan failure)
                @?= Just 1
              diagnosticMessage failure @?= "duplicate source module"
              environmentLoadErrorDiagnostics
                (DuplicateModuleDeclarations failures) @?= failures
            Left failure -> fail $ "unexpected duplicate-module failure: "
              ++ show failure
            Right _ -> fail "duplicate explicit modules were accepted"
      , testCase "checked loading rejects duplicate implicit Main modules" $ do
          let firstPath = "/virtual/FirstMain.hs"
              secondPath = "/virtual/SecondMain.hs"
              sources =
                [ (firstPath, "first :: a -> a\n")
                , (secondPath, "second :: a -> a\n")
                ]
          LoadReport result diagnostics <- environmentFromSources sources []
          diagnostics @?= []
          case result of
            Left (DuplicateModuleDeclarations (failure :| rest)) -> do
              rest @?= []
              diagnosticCode failure @?= Just "EXF_MODULE_DUPLICATE"
              diagnosticSource failure @?= Just secondPath
              diagnosticContext failure @?=
                [ "Main is declared by both " ++ firstPath
                    ++ " and " ++ secondPath
                ]
            Left failure -> fail $ "unexpected duplicate-Main failure: "
              ++ show failure
            Right _ -> fail "duplicate implicit Main modules were accepted"
      , testCase "ordinary import cycles fail independently of graph parity" $ do
          let cyclicSources =
                [ ("B.hs", unlines
                    [ "module B (T) where"
                    , "data T = BT"
                    ])
                , ("D.hs", unlines
                    [ "module D (T) where"
                    , "data T = DT"
                    ])
                , ("A.hs", unlines
                    [ "module A (T) where"
                    , "import B"
                    , "import C"
                    ])
                , ("C.hs", unlines
                    [ "module C (T) where"
                    , "import D"
                    , "import A"
                    ])
                , ("E.hs", unlines
                    [ "module E where"
                    , "import A"
                    , "selected :: T"
                    ])
                ]
              unrelated = ("F.hs", "module F where\n")
              loadCycle sources = do
                LoadReport result diagnostics <- parseModuleSources sources
                diagnostics @?= []
                case result of
                  Left (CyclicModuleImports (failure :| rest)) -> do
                    rest @?= []
                    environmentLoadErrorDiagnostics
                      (CyclicModuleImports $ failure :| []) @?=
                        failure :| []
                    pure failure
                  Left failure -> fail $ "unexpected cycle failure: "
                    ++ show failure
                  Right _ -> fail "an ordinary import cycle was accepted"
          baseFailure <- loadCycle cyclicSources
          extendedFailure <- loadCycle $ cyclicSources ++ [unrelated]
          extendedFailure @?= baseFailure
          diagnosticCode baseFailure @?= Just "EXF_MODULE_CYCLE"
          diagnosticSource baseFailure @?= Just "C.hs"
          fmap (sourceLine . sourceStart) (diagnosticSpan baseFailure)
            @?= Just 3
          diagnosticContext baseFailure @?= ["A -> C -> A"]
      , testCase "SOURCE imports break ordinary module cycles" $ do
          _ <- expectSourceEnvironment
            [ ("A.hs", unlines
                [ "module A where"
                , "import {-# SOURCE #-} B"
                , "data A = A"
                ])
            , ("B.hs", unlines
                [ "module B where"
                , "import A"
                , "data B = B"
                ])
            ]
          pure ()
      , testGroup "source declaration import scope"
        [ testCase "a direct import disambiguates an unqualified type" $ do
            environment <- expectSourceEnvironment
              [ ("A.hs", unlines
                  [ "module A where"
                  , "data T = AT"
                  ])
              , ("B.hs", unlines
                  [ "module B where"
                  , "data T = BT"
                  ])
              , ("Use.hs", unlines
                  [ "module Use where"
                  , "import A (T)"
                  , "selected :: T -> T"
                  ])
              ]
            selected <- sourceFunctionNamed environment
              $ validQualifiedName ["Use"] "selected"
            let imported = TypeCons $ validQualifiedName ["A"] "T"
            functionParameters selected @?= [imported]
            functionResult selected @?= imported
        , testCase "class constraints use the same direct import scope" $ do
            environment <- expectSourceEnvironment
              [ ("A.hs", unlines
                  [ "module A where"
                  , "class C a"
                  ])
              , ("B.hs", unlines
                  [ "module B where"
                  , "class C a"
                  ])
              , ("Use.hs", unlines
                  [ "module Use where"
                  , "import A (C)"
                  , "selected :: C a => a -> a"
                  ])
              ]
            selected <- sourceFunctionNamed environment
              $ validQualifiedName ["Use"] "selected"
            case functionConstraints selected of
              [HsConstraint owner [TypeVar _]] ->
                owner @?= validQualifiedName ["A"] "C"
              constraints -> fail $ "unexpected imported constraints: "
                ++ show constraints
        , testCase "scope threads through every declaration type pass" $ do
            environment <- expectSourceEnvironment
              [ ("A.hs", unlines
                  [ "module A where"
                  , "data T = AT"
                  , "class C a"
                  ])
              , ("B.hs", unlines
                  [ "module B where"
                  , "data T = BT"
                  , "class C a"
                  ])
              , ("Use.hs", unlines
                  [ "module Use where"
                  , "import A (T, C)"
                  , "type Alias = T"
                  , "data Box = Box T"
                  , "class Owner a where"
                  , "  method :: T -> a"
                  , "class D a"
                  , "instance C T => D T"
                  ])
              ]
            let importedType = TypeCons $ validQualifiedName ["A"] "T"
                importedClass = validQualifiedName ["A"] "C"
                localClass = validQualifiedName ["Use"] "D"
            case find
                ((== validQualifiedName ["Use"] "Alias") . tdecl_name)
                (sourceTypeSynonyms environment) of
              Just declaration -> tdecl_result declaration @?= importedType
              Nothing -> fail "import-scoped type synonym was not loaded"
            constructor <- sourceFunctionNamed environment
              $ validQualifiedName ["Use"] "Box"
            functionParameters constructor @?= [importedType]
            method <- sourceFunctionNamed environment
              $ validQualifiedName ["Use"] "method"
            functionParameters method @?= [importedType]
            let instances = concat $ Map.elems $ sClassEnv_instances
                  $ sourceClasses environment
                matchingInstances =
                  [ declaration
                  | declaration <- instances
                  , constraint_tclass (instance_head declaration) == localClass
                  , constraint_params (instance_head declaration)
                      == [importedType]
                  ]
            case matchingInstances of
              declaration : _ -> assertBool (show declaration)
                $ HsConstraint importedClass [importedType]
                    `elem` instance_constraints declaration
              [] -> fail "import-scoped instance head was not loaded"
        , testCase "qualified aliases preserve canonical identity and lists" $ do
            environment <- expectSourceEnvironment
              [ ("A.hs", unlines
                  [ "module A where"
                  , "data T = AT"
                  , "data U = AU"
                  ])
              , ("Use.hs", unlines
                  [ "module Use where"
                  , "import qualified A as X (T)"
                  , "selected :: X.T"
                  ])
              ]
            selected <- sourceFunctionNamed environment
              $ validQualifiedName ["Use"] "selected"
            functionResult selected @?=
              TypeCons (validQualifiedName ["A"] "T")
            messages <- expectBindingScopeFailure
              [ ("A.hs", unlines
                  [ "module A where"
                  , "data T = AT"
                  , "data U = AU"
                  ])
              , ("Use.hs", unlines
                  [ "module Use where"
                  , "import qualified A as X (T)"
                  , "excluded :: X.U"
                  ])
              ]
            assertBool (show messages)
              $ any ("X.U is not in scope" `isInfixOf`) messages
        , testCase "a local declaration wins over imported names" $ do
            environment <- expectSourceEnvironment
              [ ("A.hs", "module A where\ndata T = AT\n")
              , ("B.hs", "module B where\ndata T = BT\n")
              , ("Use.hs", unlines
                  [ "module Use where"
                  , "import A (T)"
                  , "import B (T)"
                  , "data T = LocalT"
                  , "selected :: T"
                  ])
              ]
            selected <- sourceFunctionNamed environment
              $ validQualifiedName ["Use"] "selected"
            functionResult selected @?=
              TypeCons (validQualifiedName ["Use"] "T")
        , testCase "hiding and missing imports reject loaded declarations" $ do
            hidden <- expectBindingScopeFailure
              [ ("A.hs", "module A where\ndata T = AT\n")
              , ("Use.hs", unlines
                  [ "module Use where"
                  , "import A hiding (T)"
                  , "hidden :: T"
                  ])
              ]
            assertBool (show hidden)
              $ any ("T is not in scope" `isInfixOf`) hidden
            missing <- expectBindingScopeFailure
              [ ("A.hs", "module A where\ndata T = AT\n")
              , ("Support.hs", "module Support where\ndata U = U\n")
              , ("Use.hs", unlines
                  [ "module Use where"
                  , "import Support ()"
                  , "bare :: T"
                  , "qualified :: A.T"
                  ])
              ]
            case missing of
              [bareFailure, qualifiedFailure] -> do
                assertBool (show missing)
                  $ "T is not in scope" `isInfixOf` bareFailure
                assertBool (show missing)
                  $ "A.T is not in scope" `isInfixOf` qualifiedFailure
              _ -> fail $ "unexpected missing-import failures: "
                ++ show missing
        , testCase "public source loaders are strict with no imports" $ do
            let sources =
                  [ ("A.hs", "module A where\ndata T = AT\n")
                  , ("Use.hs", unlines
                      [ "module Use where"
                      , "excluded :: T"
                      ])
                  ]
            messages <- expectBindingScopeFailure sources
            assertBool (show messages)
              $ any ("T is not in scope" `isInfixOf`) messages
            LoadReport checkedResult _ <- environmentFromSources sources []
            case checkedResult of
              Left (BindingDeclarationErrors failures) -> assertBool
                (show failures)
                $ any (("T is not in scope" `isInfixOf`)
                    . extractionErrorMessage)
                $ NonEmpty.toList failures
              Left failure -> fail $ "unexpected snapshot scope failure: "
                ++ show failure
              Right _ -> fail "checked snapshot loading used global fallback"
        , testCase "explicit exports retain named re-export identity" $ do
            environment <- expectSourceEnvironment
              [ ("A.hs", unlines
                  [ "module A (T) where"
                  , "data T = AT"
                  , "data Private = Private"
                  ])
              , ("Reexport.hs", unlines
                  [ "module Reexport (T) where"
                  , "import A (T)"
                  ])
              , ("Use.hs", unlines
                  [ "module Use where"
                  , "import Reexport (T)"
                  , "selected :: T"
                  ])
              ]
            selected <- sourceFunctionNamed environment
              $ validQualifiedName ["Use"] "selected"
            functionResult selected @?=
              TypeCons (validQualifiedName ["A"] "T")
            privateFailure <- expectBindingScopeFailure
              [ ("A.hs", unlines
                  [ "module A (T) where"
                  , "data T = AT"
                  , "data Private = Private"
                  ])
              , ("Use.hs", unlines
                  [ "module Use where"
                  , "import A (Private)"
                  , "excluded :: Private"
                  ])
              ]
            assertBool (show privateFailure)
              $ any ("Private is not in scope" `isInfixOf`) privateFailure
        , testCase "module self exports include local declarations" $ do
            environment <- expectSourceEnvironment
              [ ("Self.hs", unlines
                  [ "module Self (module Self) where"
                  , "data Local = LocalConstructor"
                  ])
              , ("Use.hs", unlines
                  [ "module Use where"
                  , "import Self (Local)"
                  , "selected :: Local"
                  ])
              ]
            selected <- sourceFunctionNamed environment
              $ validQualifiedName ["Use"] "selected"
            functionResult selected @?=
              TypeCons (validQualifiedName ["Self"] "Local")
        , testCase "module exports honor aliased and unaliased imports" $ do
            environment <- expectSourceEnvironment
              [ ("Origin.hs", unlines
                  [ "module Origin where"
                  , "data Direct = DirectConstructor"
                  , "data Aliased = AliasedConstructor"
                  ])
              , ("Direct.hs", unlines
                  [ "module Direct (module Origin) where"
                  , "import Origin (Direct)"
                  ])
              , ("Aliased.hs", unlines
                  [ "module Aliased (module X) where"
                  , "import Origin as X (Aliased)"
                  ])
              , ("Use.hs", unlines
                  [ "module Use where"
                  , "import Direct (Direct)"
                  , "import Aliased (Aliased)"
                  , "direct :: Direct"
                  , "aliased :: Aliased"
                  ])
              ]
            direct <- sourceFunctionNamed environment
              $ validQualifiedName ["Use"] "direct"
            functionResult direct @?=
              TypeCons (validQualifiedName ["Origin"] "Direct")
            aliased <- sourceFunctionNamed environment
              $ validQualifiedName ["Use"] "aliased"
            functionResult aliased @?=
              TypeCons (validQualifiedName ["Origin"] "Aliased")
        , testCase "qualified-only module aliases re-export no names" $ do
            failures <- expectBindingScopeFailure
              [ ("Origin.hs", unlines
                  [ "module Origin where"
                  , "data T = TConstructor"
                  ])
              , ("QualifiedOnly.hs", unlines
                  [ "module QualifiedOnly (module X) where"
                  , "import qualified Origin as X (T)"
                  ])
              , ("Use.hs", unlines
                  [ "module Use where"
                  , "import QualifiedOnly (T)"
                  , "excluded :: T"
                  ])
              ]
            assertBool (show failures)
              $ any ("T is not in scope" `isInfixOf`) failures
        , testCase "module exports intersect identity across imports" $ do
            environment <- expectSourceEnvironment
              [ ("Origin.hs", unlines
                  [ "module Origin where"
                  , "data T = TConstructor"
                  ])
              , ("Bridge.hs", unlines
                  [ "module Bridge (module X) where"
                  , "import Origin (T)"
                  , "import qualified Origin as X (T)"
                  ])
              , ("Use.hs", unlines
                  [ "module Use where"
                  , "import Bridge (T)"
                  , "selected :: T"
                  ])
              ]
            selected <- sourceFunctionNamed environment
              $ validQualifiedName ["Use"] "selected"
            functionResult selected @?=
              TypeCons (validQualifiedName ["Origin"] "T")
        , testCase "positive lists preserve unloaded external identities" $ do
            environment <- expectSourceEnvironment
              [ ("Use.hs", unlines
                  [ "module Use where"
                  , "import qualified Data.Text as X (Text)"
                  , "selected :: X.Text"
                  ])
              ]
            selected <- sourceFunctionNamed environment
              $ validQualifiedName ["Use"] "selected"
            functionResult selected @?=
              TypeCons (validQualifiedName ["Data", "Text"] "Text")
        , testCase "external hiding lists block bare and qualified names" $
            forM_
              [ ("import Data.External hiding (Hidden)", "Hidden")
              , ( "import qualified Data.External as X hiding (Hidden)"
                , "X.Hidden"
                )
              ] $ \(importDeclaration, typeName) -> do
                failures <- expectBindingScopeFailure
                  [ ("Use.hs", unlines
                      [ "module Use where"
                      , importDeclaration
                      , "excluded :: " ++ typeName
                      ])
                  ]
                assertBool (typeName ++ ": " ++ show failures)
                  $ any ("is not in scope" `isInfixOf`) failures
        , testCase "an external hiding route does not block another import" $ do
            environment <- expectSourceEnvironment
              [ ("Origin.hs", unlines
                  [ "module Origin where"
                  , "data Shared = SharedConstructor"
                  ])
              , ("Use.hs", unlines
                  [ "module Use where"
                  , "import Data.External hiding (Shared)"
                  , "import Origin (Shared)"
                  , "selected :: Shared"
                  ])
              ]
            selected <- sourceFunctionNamed environment
              $ validQualifiedName ["Use"] "selected"
            functionResult selected @?=
              TypeCons (validQualifiedName ["Origin"] "Shared")
        , testCase "a loaded alias does not close an external alias" $ do
            environment <- expectSourceEnvironment
              [ ("Loaded.hs", unlines
                  [ "module Loaded where"
                  , "data Admitted = AdmittedConstructor"
                  ])
              , ("Use.hs", unlines
                  [ "module Use where"
                  , "import qualified Loaded as X (Admitted)"
                  , "import qualified Data.External as X"
                  , "selected :: X.External"
                  ])
              ]
            selected <- sourceFunctionNamed environment
              $ validQualifiedName ["Use"] "selected"
            functionResult selected @?=
              TypeCons
                (validQualifiedName ["Data", "External"] "External")
        , testCase "package imports fail before nominal scope projection" $ do
            LoadReport result _ <- parseModuleSources
              [ ("A.hs", "module A where\ndata T = AT\n")
              , ("Use.hs", unlines
                  [ "{-# LANGUAGE PackageImports #-}"
                  , "module Use where"
                  , "import \"package\" A (T)"
                  , "excluded :: T"
                  ])
              ]
            occurrences <- expectUnsupportedVocabulary result
            map unsupportedVocabularyForm occurrences @?=
              [PackageQualifiedImport]
            map (diagnosticSource . unsupportedVocabularyDiagnostic)
              occurrences @?= [Just "Use.hs"]
        , testCase "loaded Prelude is implicit unless disabled" $ do
            let prelude = ("Prelude.hs", unlines
                  [ "module Prelude (Bool) where"
                  , "data Bool = False | True"
                  ])
                support = ("Support.hs",
                  "module Support where\ndata Marker = Marker\n")
            environment <- expectSourceEnvironment
              [ prelude
              , support
              , ("Use.hs", unlines
                  [ "module Use where"
                  , "import Support ()"
                  , "selected :: Bool"
                  ])
              ]
            selected <- sourceFunctionNamed environment
              $ validQualifiedName ["Use"] "selected"
            functionResult selected @?=
              TypeCons (validQualifiedName ["Prelude"] "Bool")
            disabled <- expectBindingScopeFailure
              [ prelude
              , support
              , ("Use.hs", unlines
                  [ "{-# LANGUAGE NoImplicitPrelude #-}"
                  , "module Use where"
                  , "import Support ()"
                  , "selected :: Bool"
                  ])
              ]
            assertBool (show disabled)
              $ any ("Bool is not in scope" `isInfixOf`) disabled
        , testCase "later LANGUAGE switches decide implicit Prelude" $ do
            let prelude = ("Prelude.hs", unlines
                  [ "module Prelude (Bool) where"
                  , "data Bool = False | True"
                  ])
                support = ("Support.hs",
                  "module Support where\ndata Marker = Marker\n")
                sources switches =
                  [ prelude
                  , support
                  , ("Use.hs", unlines
                      [ "{-# LANGUAGE " ++ switches ++ " #-}"
                      , "module Use where"
                      , "import Support ()"
                      , "selected :: Bool"
                      ])
                  ]
            forM_
              [ "NoImplicitPrelude, ImplicitPrelude"
              , "RebindableSyntax, NoRebindableSyntax"
              ] $ \switches -> do
                _ <- expectSourceEnvironment $ sources switches
                pure ()
            forM_
              [ "ImplicitPrelude, NoImplicitPrelude"
              , "NoRebindableSyntax, RebindableSyntax"
              ] $ \switches -> do
                failures <- expectBindingScopeFailure $ sources switches
                assertBool (switches ++ ": " ++ show failures)
                  $ any ("Bool is not in scope" `isInfixOf`) failures
        , testCase "later OPTIONS_GHC switches decide implicit Prelude" $ do
            let prelude = ("Prelude.hs", unlines
                  [ "module Prelude (Bool) where"
                  , "data Bool = False | True"
                  ])
                sources switches =
                  [ prelude
                  , ("Use.hs", unlines
                      [ "{-# OPTIONS_GHC " ++ switches ++ " #-}"
                      , "module Use where"
                      , "selected :: Bool"
                      ])
                  ]
            forM_
              [ "-XNoImplicitPrelude -XImplicitPrelude"
              , "-fno-implicit-prelude -fimplicit-prelude"
              ] $ \switches -> do
                _ <- expectSourceEnvironment $ sources switches
                pure ()
            forM_
              [ "-XImplicitPrelude -XNoImplicitPrelude"
              , "-fimplicit-prelude -fno-implicit-prelude"
              ] $ \switches -> do
                failures <- expectBindingScopeFailure $ sources switches
                assertBool (switches ++ ": " ++ show failures)
                  $ any ("Bool is not in scope" `isInfixOf`) failures
        , testCase "parse-mode flags can suppress implicit Prelude" $
            withTemporaryFile (unlines
              [ "module Prelude (Bool) where"
              , "data Bool = False | True"
              ]) $ \preludePath ->
            withTemporaryFile (unlines
              [ "module ModeDisabled where"
              , "excluded :: Bool"
              ]) $ \usePath -> do
              let preludeMode = haskellSrcExtsParseMode preludePath
                  baseUseMode = haskellSrcExtsParseMode usePath
                  disabledMode = baseUseMode
                    { HSE.extensions =
                        HSE.DisableExtension HSE.ImplicitPrelude
                          : HSE.extensions baseUseMode
                    }
              LoadReport result _ <- parseModules
                [(preludeMode, preludePath), (disabledMode, usePath)]
              case result of
                Left (BindingDeclarationErrors failures) -> assertBool
                  (show failures)
                  $ any (("Bool is not in scope" `isInfixOf`)
                      . extractionErrorMessage)
                  $ NonEmpty.toList failures
                Left failure -> fail $ "unexpected parse-mode failure: "
                  ++ show failure
                Right _ -> fail "parse mode ignored disabled implicit Prelude"
        , testCase "legacy extraction keeps unique-global fallback" $ do
            provider <- expectParsedModule
              "module Provider where\ndata T = T\n"
            consumer <- expectParsedModule
              "module Consumer where\nlegacy :: T -> T\n"
            typeNames <- expectRight $ getDataTypes [provider]
            results <- pure $ runIdentity
              $ getDecls typeNames Map.empty Map.empty [consumer]
            legacy <- case results of
              [result] -> expectRight result
              _ -> fail $ "unexpected legacy extraction results: "
                ++ show results
            let imported = TypeCons $ validQualifiedName ["Provider"] "T"
            functionParameters legacy @?= [imported]
            functionResult legacy @?= imported
        ]
      , testCase "mixed binding categories retain declaration source order" $
          withTemporaryFile (unlines
            [ "module OrderedBindings where"
            , "ordinaryBefore, ordinaryPeer :: a -> a"
            , "class Owner a where"
            , "  method, methodPeer :: a -> a"
            , "data Box a = Box a"
            , "ordinaryAfter :: a -> a"
            ]) $ \modulePath -> do
              LoadReport result _ <- parseModules
                [(haskellSrcExtsParseMode modulePath, modulePath)]
              environment <- expectRight result
              ordinaryBefore <- expectRight
                $ mkQualifiedName ["OrderedBindings"] "ordinaryBefore"
              ordinaryPeer <- expectRight
                $ mkQualifiedName ["OrderedBindings"] "ordinaryPeer"
              owner <- expectRight
                $ mkQualifiedName ["OrderedBindings"] "Owner"
              method <- expectRight
                $ mkQualifiedName ["OrderedBindings"] "method"
              methodPeer <- expectRight
                $ mkQualifiedName ["OrderedBindings"] "methodPeer"
              box <- expectRight
                $ mkQualifiedName ["OrderedBindings"] "Box"
              ordinaryAfter <- expectRight
                $ mkQualifiedName ["OrderedBindings"] "ordinaryAfter"
              let expectedNames =
                    [ ordinaryBefore, ordinaryPeer
                    , method, methodPeer
                    , box, ordinaryAfter
                    ]
                  sourceDeclarations = filter
                    ((`elem` expectedNames)
                      . functionName . sourceBindingFunction)
                    $ sourceBindings environment
              map (functionName . sourceBindingFunction) sourceDeclarations
                @?= expectedNames
              case sourceDeclarations of
                [ SourceFunction _
                  , SourceFunction _
                  , SourceClassMethod firstOwner _
                  , SourceClassMethod actualOwner _
                  , SourceFunction _
                  , SourceFunction _
                  ] -> do
                    firstOwner @?= owner
                    actualOwner @?= owner
                declarations -> fail $ "unexpected source binding tags: "
                  ++ show declarations
      , testCase "mixed binding errors retain declaration source order" $
          withTemporaryFile (unlines
            [ "{-# LANGUAGE TypeFamilies #-}"
            , "module OrderedBindingErrors where"
            , "ordinaryBefore, ordinaryPeer :: Int ~ Bool"
            , "class Owner a where"
            , "  method :: Int ~ Bool"
            , "data Broken = Broken (Int ~ Bool)"
            , "ordinaryAfter :: Bool ~ Int"
            ]) $ \modulePath -> do
              LoadReport result _ <- parseModules
                [(haskellSrcExtsParseMode modulePath, modulePath)]
              (failureGroup, failures) <- case result of
                Left (BindingDeclarationErrors
                    group@(firstFailure :| remaining)) ->
                  pure (group, firstFailure : remaining)
                Left failure -> fail $ "unexpected load failure: "
                  ++ show failure
                Right _ -> fail "malformed binding declarations were accepted"
              let failureLine failure = do
                    location <- extractionErrorLocation failure
                    pure $ sourceLine $ sourceStart $ locationSpan location
              map failureLine failures @?= map Just [3, 5, 6, 7]
              let rendered = NonEmpty.toList
                    $ environmentLoadErrorDiagnostics
                    $ BindingDeclarationErrors failureGroup
              map (fmap (sourceLine . sourceStart) . diagnosticSpan) rendered
                @?= map Just [3, 5, 6, 7]
      , testCase "missing modules produce source-bearing read errors" $ do
          environmentDirectory <- getDataFileName "exference/environment"
          let modulePath = environmentDirectory ++ "/missing-module.hs"
              ratingPath = environmentDirectory ++ "/all.ratings"
          (result, messages) <- runLoad
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
          environmentDirectory <- getDataFileName "exference/environment"
          let missingDirectory = environmentDirectory ++ "/missing-environment"
          (result, messages) <- runLoad
            $ environmentFromPath missingDirectory
          case result of
            Left (EnvironmentDirectoryReadError readError) ->
              diagnosticSource readError @?= Just missingDirectory
            Left failure -> fail $ "unexpected load failure: " ++ show failure
            Right _ -> fail "a missing environment directory was accepted"
          assertBool ("failed directory load emitted messages: " ++ show messages)
            $ null messages
      , testCase "existing loader diagnostics retain source structure" $ do
          let preserved = withSpan
                (validSourceSpan 3 5 3 9)
                $ withSource "Fixture.hs"
                $ withCode "EXF_PRESERVED"
                $ (diagnostic "original diagnostic")
                    {diagnosticSeverity = Warning}
              occurrence = UnsupportedVocabularyOccurrence
                OpenTypeFamily preserved
              expectedDirectory =
                withCode "EXF_ENV_DIRECTORY_READ" preserved :| []
              expectedModule = withCode "EXF_MODULE_READ" preserved :| []
              expectedParse = withCode "EXF_MODULE_PARSE" preserved :| []
              expectedUnsupported = preserved :| []
          environmentLoadErrorDiagnostics
            (EnvironmentDirectoryReadError preserved) @?= expectedDirectory
          environmentLoadErrorDiagnostics
            (ModuleReadErrors $ preserved :| []) @?= expectedModule
          environmentLoadErrorDiagnostics
            (ModuleParseErrors $ preserved :| []) @?= expectedParse
          environmentLoadErrorDiagnostics
            (UnsupportedSourceVocabulary $ occurrence :| [])
              @?= expectedUnsupported
      , testCase "legacy string loader phases receive structured diagnostics" $ do
          className <- expectRight $ mkQualifiedName ["Fixture"] "Class"
          let cases =
                [ ( DataTypeNameError $ extractionError "name detail"
                  , "EXF_DATA_TYPE_NAME"
                  , "could not extract source data-type names"
                  , "name detail"
                  )
                , ( TypeDeclarationErrors
                      $ extractionError "type detail" :| []
                  , "EXF_TYPE_DECLARATION"
                  , "could not load a source type declaration"
                  , "type detail"
                  )
                , ( ClassEnvironmentLoadFailure
                      $ ClassDeclarationErrors
                      $ extractionError "class detail" :| []
                  , "EXF_CLASS_DECLARATION"
                  , "could not load a source class declaration"
                  , "class detail"
                  )
                , ( ClassEnvironmentLoadFailure
                      $ InstanceDeclarationErrors
                      $ extractionError "instance detail" :| []
                  , "EXF_INSTANCE_DECLARATION"
                  , "could not load a source instance declaration"
                  , "instance detail"
                  )
                , ( ClassEnvironmentLoadFailure
                      $ InvalidClassEnvironment $ InvalidClassName className
                  , "EXF_CLASS_ENVIRONMENT"
                  , "the source class environment failed nominal validation"
                  , show $ InvalidClassName className
                  )
                , ( BindingDeclarationErrors
                      $ extractionError "binding detail" :| []
                  , "EXF_BINDING_DECLARATION"
                  , "could not load a source binding declaration"
                  , "binding detail"
                  )
                , ( BuiltInEnvironmentErrors $ "built-in detail" :| []
                  , "EXF_BUILTIN_ENVIRONMENT"
                  , "could not construct Exference's built-in source environment"
                  , "built-in detail"
                  )
                , ( InvalidSourceInventory
                      $ SynthesisEnvironmentDeclarationError
                        ExpectedValueDeclaration
                  , "EXF_SOURCE_INVENTORY"
                  , "the source environment failed shared inventory validation"
                  , show (SynthesisEnvironmentDeclarationError
                      ExpectedValueDeclaration)
                  )
                ]
          mapM_ (\(failure, code, message, detail) -> case
              environmentLoadErrorDiagnostics failure of
            value :| [] -> do
              diagnosticSeverity value @?= Error
              diagnosticCode value @?= Just code
              diagnosticMessage value @?= message
              diagnosticContext value @?= [detail]
            values -> fail $ "unexpected diagnostics: " ++ show values
            ) cases
      , testCase "stable session loader hides frontend load failures" $ do
          environmentDirectory <- getDataFileName "exference/environment"
          let missingDirectory = environmentDirectory ++ "/missing-session"
          ExferenceSessionLoadReport result diagnostics <-
            loadExferenceSession missingDirectory
          diagnostics @?= []
          case result of
            Left (failure :| []) ->
              ( diagnosticCode failure
              , diagnosticSource failure
              ) @?= (Just "EXF_ENV_DIRECTORY_READ", Just missingDirectory)
            Left failures -> fail $ "unexpected diagnostics: " ++ show failures
            Right _ -> fail "a missing session environment was accepted"
      , testCase "policy-aware session loader includes omission diagnostics" $ do
          environmentDirectory <- getDataFileName "exference/environment"
          excluded <- expectRight $ SharedName.parseName "Data.Function.fix"
          let policy = defaultExferenceSessionPolicy
                {exferenceExcludedBindings = [excluded]}
          ExferenceSessionLoadReport result diagnostics <-
            loadExferenceSessionWithPolicy policy environmentDirectory
          _ <- expectRight result
          length (filter ((== Info) . diagnosticSeverity) diagnostics)
            @?= 5
          assertBool ("missing policy diagnostic: " ++ show diagnostics)
            $ Just "DJEX_EXF_POLICY_OMISSION"
                `elem` map diagnosticCode diagnostics
      , testCase
          "explicit file loading preserves module order and applies ratings" $
          withTemporaryFile (unlines
            [ "module ExplicitSecond where"
            , "second :: a -> a"
            ]) $ \secondPath ->
          withTemporaryFile (unlines
            [ "module ExplicitFirst where"
            , "first :: a -> a"
            ]) $ \firstPath ->
          withTemporaryFile "ExplicitFirst.first 1.5" $ \firstRatingPath ->
          withTemporaryFile "ExplicitSecond.second 2.5"
              $ \secondRatingPath -> do
            LoadReport result _ <- environmentFromFiles
              [secondPath, firstPath]
              [firstRatingPath, secondRatingPath]
            checked <- expectRight result
            secondName <- expectRight
              $ mkQualifiedName ["ExplicitSecond"] "second"
            firstName <- expectRight
              $ mkQualifiedName ["ExplicitFirst"] "first"
            let explicitBindings = filter
                  ((`elem` [secondName, firstName]) . functionName)
                  $ sourceFunctions $ checkedSourceProjection checked
            map (\binding -> (functionName binding, functionPenalty binding))
                explicitBindings @?=
              [ (secondName, Penalty 2.5)
              , (firstName, Penalty 1.5)
              ]
      , testCase
          "in-memory source loading retains paths, order, and ratings" $ do
          let modulePath = "/virtual/SnapshotModule.hs"
              ratingPath = "/virtual/SnapshotModule.ratings"
              moduleSource = unlines
                [ "module SnapshotModule where"
                , "snapshotIdentity :: a -> a"
                ]
              ratingSource = "SnapshotModule.snapshotIdentity 4.25"
          LoadReport result _ <- environmentFromSources
            [(modulePath, moduleSource)] [(ratingPath, ratingSource)]
          checked <- expectRight result
          identityName <- expectRight
            $ mkQualifiedName ["SnapshotModule"] "snapshotIdentity"
          let matching = filter ((== identityName) . functionName)
                $ sourceFunctions $ checkedSourceProjection checked
          map functionPenalty matching @?= [Penalty 4.25]
          ExferenceSessionLoadReport sessionResult _ <-
            loadExferenceSessionFromSources
              [(modulePath, moduleSource)] [(ratingPath, ratingSource)]
          _ <- expectRight sessionResult
          pure ()
      , testCase "in-memory parse diagnostics retain path order" $ do
          let modulePaths =
                [ "/virtual/FirstBrokenSnapshot.hs"
                , "/virtual/SecondBrokenSnapshot.hs"
                ]
          LoadReport result _ <- environmentFromSources
            [ (path, "module BrokenSnapshot where\nbroken ::")
            | path <- modulePaths
            ] []
          case result of
            Left (ModuleParseErrors failures) ->
              map diagnosticSource (NonEmpty.toList failures)
                @?= map Just modulePaths
            Left failure -> fail $ "unexpected source failure: " ++ show failure
            Right _ -> fail "a malformed in-memory module was accepted"
      , testCase "unused package imports fail after LANGUAGE pragma parsing" $ do
          let modulePath = "/virtual/PackageImportSnapshot.hs"
              moduleSource = unlines
                [ "{-# LANGUAGE PackageImports #-}"
                , "module PackageImportSnapshot where"
                , "import \"base\" Data.Maybe"
                , "snapshotIdentity :: a -> a"
                ]
          LoadReport result _ <- environmentFromSources
            [(modulePath, moduleSource)] []
          occurrences <- expectUnsupportedVocabulary result
          map unsupportedVocabularyForm occurrences @?=
            [PackageQualifiedImport]
          map (diagnosticSource . unsupportedVocabularyDiagnostic)
            occurrences @?= [Just modulePath]
      , testCase "explicit files accept no modules and order rating warnings" $
          do
            environmentDirectory <- getDataFileName "exference/environment"
            let secondMissing =
                  environmentDirectory ++ "/second-missing.ratings"
                firstMissing = environmentDirectory ++ "/first-missing.ratings"
            LoadReport result diagnostics <- environmentFromFiles []
              [secondMissing, firstMissing]
            checked <- expectRight result
            assertBool "the empty module set lost built-in constructors"
              $ not $ null $ sourceFunctions $ checkedSourceProjection checked
            map diagnosticSource
                (filter ((== Warning) . diagnosticSeverity) diagnostics) @?=
              [Just secondMissing, Just firstMissing]
      , testCase "explicit file session loading applies policy" $
          withTemporaryFile (unlines
            [ "module ExplicitPolicy where"
            , "hidden :: a -> a"
            ]) $ \modulePath -> do
              hiddenName <- expectRight
                $ SharedName.parseName "ExplicitPolicy.hidden"
              let policy = defaultExferenceSessionPolicy
                    {exferenceExcludedBindings = [hiddenName]}
              ExferenceSessionLoadReport result diagnostics <-
                loadExferenceSessionFromFilesWithPolicy
                  policy [modulePath] []
              session <- expectRight result
              map (\omission -> (omittedName omission, omittedReason omission))
                  (filter ((== hiddenName) . omittedName)
                    $ exferenceSessionOmissions session) @?=
                [(hiddenName, ExcludedByPolicy)]
              assertBool ("missing explicit-file policy diagnostic: "
                  ++ show diagnostics)
                $ Just "DJEX_EXF_POLICY_OMISSION"
                    `elem` map diagnosticCode diagnostics
              ExferenceSessionLoadReport emptyResult _ <-
                loadExferenceSessionFromFiles [] []
              _ <- expectRight emptyResult
              pure ()
      , testCase "single-module loading seals neutral ratings" $
          withTemporaryFile (unlines
            [ "module Neutral where"
            , "identity :: a -> a"
            ]) $ \modulePath -> do
              LoadReport result diagnostics <- environmentFromModule modulePath
              checked <- expectRight result
              identityName <- expectRight
                $ mkQualifiedName ["Neutral"] "identity"
              case find ((== identityName) . functionName)
                  (sourceFunctions $ checkedSourceProjection checked) of
                Just binding -> functionPenalty binding @?= Penalty 0
                Nothing -> fail "neutral module lost its identity declaration"
              length (filter ((== Info) . diagnosticSeverity) diagnostics)
                @?= 4
      , testCase
          "built-in constructor functions are the ordered deconstructor projection" $
          withTemporaryFile "module BuiltIns where\n" $ \modulePath -> do
            LoadReport result _ <- environmentFromModule modulePath
            checked <- expectRight result
            let projection = checkedSourceProjection checked
                expected =
                  [ FunctionBinding
                      { functionResult = deconstructorInput deconstructor
                      , functionName = constructorName constructor
                      , functionPenalty = 0
                      , functionConstraints = []
                      , functionParameters = constructorFields constructor
                      }
                  | deconstructor <- sourceDeconstructors projection
                  , constructor <- deconstructorConstructors deconstructor
                  ]
            sourceFunctions projection @?= expected
      , testCase "default tuple capability has an explicit operational cap" $
          withTemporaryFile "module BuiltIns where\n" $ \modulePath -> do
            LoadReport result _ <- environmentFromModule modulePath
            checked <- expectRight result
            let projection = checkedSourceProjection checked
                tupleNames = map validTupleName
                  [2 .. maximumBuiltInTupleArity]
            map functionName (sourceFunctions projection) @?=
              [ListCon, Cons, validTupleName 0] ++ tupleNames
            map (typeConstructorHead . deconstructorInput)
                (sourceDeconstructors projection) @?=
              map Just ([ListCon, validTupleName 0] ++ tupleNames)
            assertBool "tuple arity above the operational cap was materialized"
              $ validTupleName (maximumBuiltInTupleArity + 1)
                  `notElem` map functionName (sourceFunctions projection)
      , testCase "missing ratings retain zero penalties with one warning" $ do
          environmentDirectory <- getDataFileName "exference/environment"
          let modulePath = environmentDirectory ++ "/Category.hs"
              ratingPath = environmentDirectory ++ "/missing.ratings"
          LoadReport result diagnostics <-
            environmentFromModuleAndRatings modulePath ratingPath
          checkedEnvironment <- expectRight result
          let bindings = sourceFunctions
                $ checkedSourceProjection checkedEnvironment
              warnings = filter ((== Warning) . diagnosticSeverity) diagnostics
              summaries = filter ((== Info) . diagnosticSeverity) diagnostics
          assertBool "missing ratings changed a default function penalty"
            $ all ((== Penalty 0) . functionPenalty) bindings
          case warnings of
            [warning] -> do
              diagnosticSource warning @?= Just ratingPath
              assertBool ("unexpected rating warning: " ++ show warning)
                $ "could not parse rating file"
                    `isInfixOf` diagnosticMessage warning
            _ -> fail $ "expected one rating warning, got " ++ show warnings
          length summaries @?= 4
      , testCase "duplicate ratings are diagnosed and ignored" $
          withTemporaryFile (unlines
            [ "module Ratings where"
            , "identity :: a -> a"
            ]) $ \modulePath ->
          withTemporaryFile
            "Ratings.identity 1.0 Ratings.identity 2.0"
            $ \ratingPath -> do
              (result, messages) <- runLoad
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
          environmentDirectory <- getDataFileName "exference/environment"
          let modulePath = environmentDirectory ++ "/all.ratings"
          (result, messages) <- runLoad
            $ parseModules [(haskellSrcExtsParseMode modulePath, modulePath)]
          case result of
            Left ModuleParseErrors{} -> pure ()
            Left failure -> fail $ "unexpected load failure: " ++ show failure
            Right _ -> fail "a malformed module was accepted"
          assertBool ("later loader summaries survived: " ++ show messages)
            $ not $ any isLoaderSummary messages
      , testCase "module parse diagnostics retain locations and input order" $
          withTemporaryFile (unlines
            [ "module FirstBroken where"
            , "first ::"
            ]) $ \firstPath ->
          withTemporaryFile (unlines
            [ "{-# LANGUAGE TypeFamilies #-}"
            , "module UnsupportedBetween where"
            , "type family F a"
            ]) $ \unsupportedPath ->
          withTemporaryFile (unlines
            [ "module SecondBroken where"
            , ""
            , "second ="
            ]) $ \secondPath -> do
            LoadReport result diagnostics <- parseModules
              [ (haskellSrcExtsParseMode firstPath, firstPath)
              , (haskellSrcExtsParseMode unsupportedPath, unsupportedPath)
              , (haskellSrcExtsParseMode secondPath, secondPath)
              ]
            assertBool ("parse failure emitted loader diagnostics: "
                ++ show diagnostics)
              $ null diagnostics
            case result of
              Left failure@(ModuleParseErrors values) -> do
                let expected path line = withLocation path
                      (validSourceSpan line 1 line 1)
                      $ contextualDiagnostic
                          Error
                          "EXF_MODULE_PARSE"
                          "could not parse a Haskell source module"
                          "Parse error: ;"
                    expectedValues =
                      expected firstPath 3 :| [expected secondPath 4]
                values @?= expectedValues
                environmentLoadErrorDiagnostics failure @?= expectedValues
              Left failure -> fail $ "unexpected load failure: " ++ show failure
              Right _ -> fail "malformed modules were accepted"
      , testCase "unsupported top-level vocabulary fails explicitly" $
          mapM_ (\(label, extensions, pragmas, declaration, expectedForm) -> do
              occurrences <- unsupportedFromSourceWith extensions $ unlines
                $ pragmas ++ ["module Unsupported where", declaration]
              assertEqual label [expectedForm]
                $ map unsupportedVocabularyForm occurrences)
            [ ( "open type family"
              , []
              , ["{-# LANGUAGE TypeFamilies #-}"]
              , "type family F a"
              , OpenTypeFamily
              )
            , ( "closed type family"
              , []
              , ["{-# LANGUAGE TypeFamilies #-}"]
              , "type family F a where F Int = Bool"
              , ClosedTypeFamily
              )
            , ( "data family"
              , []
              , ["{-# LANGUAGE TypeFamilies #-}"]
              , "data family D a"
              , DataFamily
              )
            , ( "GADT"
              , [HSE.GADTs]
              , ["{-# LANGUAGE GADTs #-}"]
              , "data G a where G :: a -> G a"
              , GadtDeclaration
              )
            , ( "type family instance"
              , []
              , ["{-# LANGUAGE TypeFamilies #-}"]
              , "type instance F Int = Bool"
              , TypeFamilyInstance
              )
            , ( "data family instance"
              , []
              , ["{-# LANGUAGE TypeFamilies #-}"]
              , "data instance D Int = DInt"
              , DataFamilyInstance
              )
            , ( "GADT data family instance"
              , [HSE.GADTs]
              , ["{-# LANGUAGE GADTs, TypeFamilies #-}"]
              , "data instance D Int where DInt :: D Int"
              , GadtDataFamilyInstance
              )
            , ( "standalone deriving"
              , [HSE.StandaloneDeriving]
              , ["{-# LANGUAGE StandaloneDeriving #-}"]
              , "deriving instance Eq T"
              , StandaloneDeriving
              )
            , ( "declaration splice"
              , [HSE.TemplateHaskell]
              , ["{-# LANGUAGE TemplateHaskell #-}"]
              , "$(pure [])"
              , DeclarationSplice
              )
            , ( "role annotation"
              , [HSE.RoleAnnotations]
              , ["{-# LANGUAGE RoleAnnotations #-}"]
              , "type role T nominal"
              , RoleAnnotation
              )
            , ( "pattern-synonym signature"
              , [HSE.PatternSynonyms]
              , ["{-# LANGUAGE PatternSynonyms #-}"]
              , "pattern P :: T"
              , PatternSynonymSignature
              )
            , ( "deriving clause"
              , []
              , []
              , "data T = T deriving (Eq)"
              , DerivingClause
              )
            -- Regression: kinded binders on class, synonym, and explicit
            -- instance heads parse (TypeFamilies implies KindSignatures in
            -- HSE) but previously bypassed this boundary and failed later in
            -- an extractor with a span-free string.
            , ( "kinded class binder"
              , [HSE.KindSignatures]
              , ["{-# LANGUAGE KindSignatures #-}"]
              , "class Wrap (f :: * -> *)"
              , KindedClassBinder
              )
            , ( "kinded synonym binder"
              , [HSE.KindSignatures]
              , ["{-# LANGUAGE KindSignatures #-}"]
              , "type T (a :: *) = a"
              , KindedSynonymBinder
              )
            , ( "kinded instance binder"
              , [HSE.ExplicitForAll, HSE.KindSignatures]
              , ["{-# LANGUAGE ExplicitForAll, KindSignatures #-}"]
              , "instance forall (a :: *). C a"
              , KindedInstanceBinder
              )
            ]
      , testCase "default type-operator chains fail at their complete span" $ do
          occurrences <- unsupportedFromSource $ unlines
            [ "module DefaultTypeFixity where"
            , "data A = A"
            , "data B = B"
            , "data C = C"
            , "data a :<: b = Less"
            , "data a :>: b = Greater"
            , "ambiguous :: A :<: B :>: C"
            ]
          case occurrences of
            [occurrence] -> do
              unsupportedVocabularyForm occurrence @?=
                UnparenthesizedTypeOperatorChain
              let value = unsupportedVocabularyDiagnostic occurrence
              diagnosticSpan value @?= Just (validSourceSpan 7 14 7 27)
              diagnosticMessage value @?=
                "unsupported source vocabulary: "
                  ++ "unparenthesized type-operator chain"
            _ -> fail $ "expected one type-operator occurrence, got "
              ++ show occurrences
      , testCase "opposing local type fixities cannot select a grouping" $
          forM_
            [ ("infixl 6 :<:", "infixr 5 :>:")
            , ("infixr 5 :<:", "infixl 6 :>:")
            ] $ \(leftFixity, rightFixity) -> do
              occurrences <- unsupportedFromSource $ unlines
                [ "module LocalTypeFixity where"
                , "data A = A"
                , "data B = B"
                , "data C = C"
                , "data a :<: b = Less"
                , "data a :>: b = Greater"
                , leftFixity
                , rightFixity
                , "ambiguous :: A :<: B :>: C"
                ]
              map unsupportedVocabularyForm occurrences @?=
                [UnparenthesizedTypeOperatorChain]
      , testCase "explicit type-operator groupings and one application load" $
          do
            environment <- expectSourceEnvironment
              [ ("ParenthesizedTypeFixity.hs", unlines
                  [ "module ParenthesizedTypeFixity where"
                  , "data A = A"
                  , "data B = B"
                  , "data C = C"
                  , "data a :<: b = Less"
                  , "data a :>: b = Greater"
                  , "infixl 6 :<:"
                  , "infixr 5 :>:"
                  , "single :: A :<: B"
                  , "leftGrouped :: (A :<: B) :>: C"
                  , "rightGrouped :: A :<: (B :>: C)"
                  ])
              ]
            let local occurrence =
                  validQualifiedName ["ParenthesizedTypeFixity"] occurrence
                applied occurrence arguments =
                  SharedType.applyTypeArguments
                    (TypeCons $ local occurrence) arguments
                a = TypeCons $ local "A"
                b = TypeCons $ local "B"
                c = TypeCons $ local "C"
                left = applied ":<:" [a, b]
                right = applied ":>:" [b, c]
                resultOf occurrence = functionResult
                  <$> sourceFunctionNamed environment (local occurrence)
            resultOf "single" >>= (@?= left)
            resultOf "leftGrouped" >>=
              (@?= applied ":>:" [left, c])
            resultOf "rightGrouped" >>=
              (@?= applied ":<:" [a, right])
      , testCase "declaration vocabulary retains source order" $ do
          occurrences <- unsupportedFromSourceWith [HSE.PatternSynonyms]
            $ unlines
            [ "{-# LANGUAGE PatternSynonyms, TypeFamilies #-}"
            , "module Ordered (visible) where"
            , "pattern P :: visible"
            , "type family F a"
            ]
          map unsupportedVocabularyForm occurrences @?=
            [ PatternSynonymSignature
            , OpenTypeFamily
            ]
          map occurrenceStartLine occurrences @?=
            [Just 3, Just 4]
      , testCase "explicit exports retain the complete checked inventory" $
          withTemporaryFile (unlines
            [ "module Visibility (public) where"
            , "public, private :: Int"
            ]) $ \modulePath -> do
              LoadReport result _ <- environmentFromModule modulePath
              checked <- expectRight result
              publicName <- expectRight
                $ mkQualifiedName ["Visibility"] "public"
              privateName <- expectRight
                $ mkQualifiedName ["Visibility"] "private"
              let declaredNames = map functionName
                    $ sourceFunctions $ checkedSourceProjection checked
              assertBool "the explicit export was not loaded"
                $ publicName `elem` declaredNames
              -- Export lists define an interactive visibility projection, not
              -- the dependency-complete inventory needed to check the module.
              assertBool "the loader prematurely discarded a private binding"
                $ privateName `elem` declaredNames
      , testCase "class vocabulary modifiers retain nested source order" $ do
          occurrences <- unsupportedFromSourceWith [HSE.DefaultSignatures]
            $ unlines
            [ "{-# LANGUAGE DefaultSignatures, FunctionalDependencies, TypeFamilies #-}"
            , "module UnsupportedClass where"
            , "class C a b | a -> b where"
            , "  data D a"
            , "  type F a"
            , "  type F a = a"
            , "  default method :: a -> a"
            , "  method :: a -> a"
            ]
          map unsupportedVocabularyForm occurrences @?=
            [ FunctionalDependency
            , AssociatedDataFamily
            , AssociatedTypeFamily
            , AssociatedTypeDefault
            , DefaultMethodSignature
            ]
          map occurrenceStartLine occurrences @?=
            [Just 3, Just 4, Just 5, Just 6, Just 7]
      , testCase "XML page modules fail with their exact module span" $ do
          let nativeSpan = HSE.SrcSpan "Page.hs" 1 1 4 8
              location = HSE.SrcSpanInfo nativeSpan []
              page = HSE.XmlPage location
                (HSE.ModuleName location "Page") []
                (HSE.XName location "html") [] Nothing []
              occurrences = unsupportedVocabularyOccurrences [page]
              expectedDiagnostic = Diagnostic
                { diagnosticSeverity = Error
                , diagnosticCode = Just "EXF_UNSUPPORTED_VOCABULARY"
                , diagnosticSource = Just "Page.hs"
                , diagnosticSpan = Just $ validSourceSpan 1 1 4 8
                , diagnosticMessage =
                    "unsupported source vocabulary: XML page module"
                , diagnosticContext = []
                }
          occurrences @?=
            [UnsupportedVocabularyOccurrence XmlPageModule
              expectedDiagnostic]
      , testCase "XML hybrid modules fail with their exact module span" $ do
          let nativeSpan = HSE.SrcSpan "Hybrid.hs" 2 3 8 9
              location = HSE.SrcSpanInfo nativeSpan []
              hybrid = HSE.XmlHybrid location Nothing [] [] []
                (HSE.XName location "page") [] Nothing []
              occurrences = unsupportedVocabularyOccurrences [hybrid]
              expectedDiagnostic = Diagnostic
                { diagnosticSeverity = Error
                , diagnosticCode = Just "EXF_UNSUPPORTED_VOCABULARY"
                , diagnosticSource = Just "Hybrid.hs"
                , diagnosticSpan = Just $ validSourceSpan 2 3 8 9
                , diagnosticMessage =
                    "unsupported source vocabulary: XML hybrid module"
                , diagnosticContext = []
                }
          occurrences @?=
            [UnsupportedVocabularyOccurrence XmlHybridModule
              expectedDiagnostic]
      , testCase "invalid native spans remain structured diagnostics" $ do
          let pageAt nativeSpan =
                let location = HSE.SrcSpanInfo nativeSpan []
                in HSE.XmlPage location
                (HSE.ModuleName location "Page") []
                (HSE.XName location "html") [] Nothing []
          case unsupportedVocabularyOccurrences
              [pageAt $ HSE.SrcSpan "InvalidSpan.hs" 0 3 2 1] of
            [occurrence] -> do
              let value = unsupportedVocabularyDiagnostic occurrence
              diagnosticSource value @?= Just "InvalidSpan.hs"
              diagnosticSpan value @?= Nothing
              diagnosticContext value @?=
                [ "haskell-src-exts supplied an invalid source location: "
                    ++ "NonPositiveSourceLine 0"
                ]
            occurrences -> fail $ "expected one occurrence, got "
              ++ show occurrences
          case unsupportedVocabularyOccurrences
              [pageAt $ HSE.SrcSpan "InvalidEnd.hs" 4 7 4 0] of
            [occurrence] -> do
              let value = unsupportedVocabularyDiagnostic occurrence
              diagnosticSource value @?= Just "InvalidEnd.hs"
              diagnosticSpan value @?= Just (validSourceSpan 4 7 4 7)
              diagnosticContext value @?=
                [ "haskell-src-exts supplied an invalid source location: "
                    ++ "NonPositiveSourceColumn 0"
                ]
            occurrences -> fail $ "expected one occurrence, got "
              ++ show occurrences
      , testCase "typed declaration splices are covered at the HSE boundary" $ do
          let nativeSpan = HSE.SrcSpan "TypedSplice.hs" 4 2 4 16
              location = HSE.SrcSpanInfo nativeSpan []
              expression = HSE.Var location $ HSE.UnQual location
                $ HSE.Ident location "declarations"
              modul = HSE.Module location Nothing [] []
                [HSE.TSpliceDecl location expression]
          map unsupportedVocabularyForm
              (unsupportedVocabularyOccurrences [modul]) @?=
            [TypedDeclarationSplice]
      , testCase "instance vocabulary modifiers retain nested source order" $ do
          occurrences <- unsupportedFromSourceWith [HSE.GADTs] $ unlines
            [ "{-# LANGUAGE FlexibleInstances, GADTs, TypeFamilies #-}"
            , "module UnsupportedInstance where"
            , "instance {-# OVERLAPPABLE #-} C Int where"
            , "  type F Int = Bool"
            , "  data D Int = DInt"
            , "  data D Bool where DBool :: D Bool"
            ]
          map unsupportedVocabularyForm occurrences @?=
            [ InstanceOverlapMode
            , AssociatedTypeInstance
            , AssociatedDataInstance
            , AssociatedGadtDataInstance
            ]
          map occurrenceStartLine occurrences @?=
            [Just 3, Just 4, Just 5, Just 6]
      , testCase "unsupported occurrences aggregate in module order" $
          withTemporaryFile (unlines
            [ "module FirstUnsupported where"
            , "data T = T deriving (Eq)"
            ]) $ \firstPath ->
          withTemporaryFile (unlines
            [ "{-# LANGUAGE TypeFamilies #-}"
            , "module SecondUnsupported where"
            , "type family F a"
            ]) $ \secondPath -> do
              LoadReport result _ <- parseModules
                [ (haskellSrcExtsParseMode firstPath, firstPath)
                , (haskellSrcExtsParseMode secondPath, secondPath)
                ]
              occurrences <- expectUnsupportedVocabulary result
              map unsupportedVocabularyForm occurrences @?=
                [DerivingClause, OpenTypeFamily]
              map (diagnosticSource . unsupportedVocabularyDiagnostic)
                occurrences @?= [Just firstPath, Just secondPath]
      , testCase "module parse errors precede unsupported vocabulary" $
          withTemporaryFile (unlines
            [ "{-# LANGUAGE TypeFamilies #-}"
            , "module Unsupported where"
            , "type family F a"
            ]) $ \unsupportedPath ->
          withTemporaryFile "module Broken where value ::" $ \brokenPath -> do
            LoadReport result _ <- parseModules
              [ (haskellSrcExtsParseMode unsupportedPath, unsupportedPath)
              , (haskellSrcExtsParseMode brokenPath, brokenPath)
              ]
            case result of
              Left ModuleParseErrors{} -> pure ()
              Left failure -> fail $ "unexpected load failure: " ++ show failure
              Right _ -> fail "parse failure lost precedence"
      , testCase "unsupported vocabulary precedes declaration conversion" $ do
          occurrences <- unsupportedFromSource $ unlines
            [ "{-# LANGUAGE KindSignatures, TypeFamilies #-}"
            , "module UnsupportedBeforeConversion where"
            , "type family F a"
            , "type Bad (a :: *) = a"
            ]
          -- The kinded synonym binder previously failed only during a later
          -- extraction phase; it is now itself a located vocabulary
          -- occurrence reported alongside the type family.
          map unsupportedVocabularyForm occurrences @?=
            [OpenTypeFamily, KindedSynonymBinder]
      , testCase "benign non-vocabulary declarations remain accepted" $
          withTemporaryFile (unlines
            [ "{-# LANGUAGE InstanceSigs, PatternSynonyms #-}"
            , "module Benign where"
            , "import Data.List"
            , "data T = T"
            , "infixr 5 <+>"
            , "default (T)"
            , "ordinary :: a -> a"
            , "ordinary value = value"
            , "pattern P = T"
            , "class C a where"
            , "  method :: a -> a"
            , "  method value = value"
            , "  {-# MINIMAL method #-}"
            , "instance C T where"
            , "  method :: T -> T"
            , "  method value = value"
            , "{-# SPECIALISE instance C T #-}"
            ]) $ \modulePath -> do
              let mode = enableExtensions
                    [ HSE.InstanceSigs
                    , HSE.PatternSynonyms
                    ] $ haskellSrcExtsParseMode modulePath
              LoadReport result _ <- parseModules [(mode, modulePath)]
              _ <- expectRight result
              pure ()
      , testCase "foreign imports lower through the signature path" $
          withTemporaryFile (unlines
            [ "{-# LANGUAGE ForeignFunctionInterface #-}"
            , "module Foreign where"
            , "data T = T"
            , "foreign import ccall \"foreign_value\" foreignValue :: T -> T"
            , "foreign export ccall \"foreign_exported\" exported :: T -> T"
            , "exported value = value"
            ]) $ \modulePath -> do
              let mode = enableExtensions [HSE.ForeignFunctionInterface]
                    $ haskellSrcExtsParseMode modulePath
              LoadReport result _ <- parseModules [(mode, modulePath)]
              environment <- expectRight result
              typeName <- expectRight $ mkQualifiedName ["Foreign"] "T"
              importName <- expectRight
                $ mkQualifiedName ["Foreign"] "foreignValue"
              exportName <- expectRight
                $ mkQualifiedName ["Foreign"] "exported"
              let bindings = sourceFunctions environment
              case find ((== importName) . functionName) bindings of
                Nothing -> fail "foreign import disappeared from the inventory"
                Just binding -> do
                  functionParameters binding @?= [TypeCons typeName]
                  functionResult binding @?= TypeCons typeName
                  functionConstraints binding @?= []
                  functionPenalty binding @?= Penalty 0
              assertBool "foreign export invented a second binding"
                $ all ((/= exportName) . functionName) bindings
      , testCase "record selectors receive ratings exactly once" $
          withTemporaryFile (unlines
            [ "module Rated where"
            , "data Rated = Rated { unRated :: Rated }"
            ]) $ \modulePath ->
          withTemporaryFile "Rated.unRated 7.25" $ \ratingPath -> do
            LoadReport result _ <-
              environmentFromModuleAndRatings modulePath ratingPath
            environment <- checkedSourceProjection <$> expectRight result
            selector <- expectRight
              $ mkQualifiedName ["Rated"] "unRated"
            ratedType <- expectRight
              $ mkQualifiedName ["Rated"] "Rated"
            case filter ((== selector) . functionName)
                $ sourceFunctions environment of
              [binding] -> do
                functionPenalty binding @?= Penalty 7.25
                functionParameters binding @?= [TypeCons ratedType]
                functionResult binding @?= TypeCons ratedType
              bindings -> fail $ "selector was not emitted exactly once: "
                ++ show bindings
      , testCase "unsupported datatype shapes retain exact source spans" $ do
          occurrences <- unsupportedFromSourceWith
            [ HSE.DatatypeContexts
            , HSE.ExistentialQuantification
            , HSE.KindSignatures
            ] $ unlines
            [ "{-# LANGUAGE DatatypeContexts, ExistentialQuantification, KindSignatures #-}"
            , "module UnsupportedData where"
            , "data Eq a => T (a :: *) ="
            , "    forall b. C b"
            , "  | Eq a => D a"
            ]
          map unsupportedVocabularyForm occurrences @?=
            [ DataTypeContext
            , KindedDataBinder
            , ExistentialConstructor
            , ConstrainedConstructor
            ]
          map (diagnosticSpan . unsupportedVocabularyDiagnostic) occurrences
            @?=
              [ Just $ validSourceSpan 3 6 3 13
              , Just $ validSourceSpan 3 16 3 24
              , Just $ validSourceSpan 4 5 4 18
              , Just $ validSourceSpan 5 5 5 12
              ]
      , testCase "empty contextual datatypes fail during preflight" $
          withTemporaryFile (unlines
            [ "{-# LANGUAGE EmptyDataDecls #-}"
            , "module EmptyContext where"
            , "data Eq a => Empty a"
            ]) $ \modulePath -> do
              LoadReport result _ <- parseModules
                [(haskellSrcExtsParseMode modulePath, modulePath)]
              occurrences <- expectUnsupportedVocabulary result
              map unsupportedVocabularyForm occurrences
                @?= [DataTypeContext]
              map occurrenceStartLine occurrences @?= [Just 3]
      , testCase "source LANGUAGE pragmas do not hide unsupported syntax" $
          withTemporaryFile (unlines
            [ "{-# LANGUAGE TypeFamilies #-}"
            , "module FirstUnsupported where"
            , "type family F a"
            ]) $ \modulePath -> do
              LoadReport result _ <- parseModules
                [(haskellSrcExtsParseMode modulePath, modulePath)]
              occurrences <- expectUnsupportedVocabulary result
              map unsupportedVocabularyForm occurrences @?= [OpenTypeFamily]
      , testCase "malformed ratings retain the parsed environment at defaults" $ do
          environmentDirectory <- getDataFileName "exference/environment"
          let modulePath = environmentDirectory ++ "/Category.hs"
          (sourceEnvironmentResult, messages) <- runLoad
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
          environmentDirectory <- getDataFileName "exference/environment"
          let modulePath = environmentDirectory ++ "/String.hs"
          (sourceEnvironmentResult, _) <- runLoad
            $ environmentFromModuleAndRatings modulePath modulePath
          checkedEnvironment <- expectRight sourceEnvironmentResult
          let sourceEnvironment = checkedSourceProjection checkedEnvironment
          shared <- expectRight
            $ toSynthesisSourceEnvironment sourceEnvironment
          stringName <- expectRight
            $ mkQualifiedName ["Data", "String"] "String"
          case Map.lookup stringName
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
      , testCase "frontend inventories nest each rated class method once" $
          withTemporaryFile (unlines
            [ "module Owned where"
            , "class Prerequisite a"
            , "class C a where"
            , "  method :: Prerequisite a => a -> a"
            , "ordinary :: a -> a"
            ]) $ \modulePath ->
          withTemporaryFile
            "Owned.method 2.5 Owned.ordinary 1.5"
            $ \ratingPath -> do
              LoadReport result _ <-
                environmentFromModuleAndRatings modulePath ratingPath
              checked <- expectRight result
              className <- expectRight $ mkQualifiedName ["Owned"] "C"
              prerequisiteName <- expectRight
                $ mkQualifiedName ["Owned"] "Prerequisite"
              methodName <- expectRight
                $ mkQualifiedName ["Owned"] "method"
              ordinaryName <- expectRight
                $ mkQualifiedName ["Owned"] "ordinary"
              let projection = checkedSourceProjection checked
                  inventory = checkedSourceInventory checked
                  neutral = checkedSourcePreparedInventory checked
                  backend = preparedSynthesisBackend neutral
                  shared = SharedInventory.inventoryEnvironment inventory
                  methodEntries =
                    [ (owner, binding)
                    | SourceClassMethod owner binding <-
                        sourceBindings projection
                    , functionName binding == methodName
                    ]
              SharedTypeSynonym.preparedInventory
                  (preparedSynthesisWitness neutral) @?=
                fmap (const ()) inventory
              environmentFunctions backend @?= sourceFunctions projection
              environmentDeconstructors backend @?=
                sourceDeconstructors projection
              environmentClasses backend @?= sourceClasses projection
              parameter <- case Map.lookup className
                  (sClassEnv_tclasses $ sourceClasses projection) of
                Just declaration -> case tclass_params declaration of
                  [classParameter] -> pure classParameter
                  parameters -> fail $ "unexpected class parameters: "
                    ++ show parameters
                Nothing -> fail "method owner class was not elaborated"
              case methodEntries of
                [(owner, binding)] -> do
                  owner @?= className
                  functionPenalty binding @?= Penalty 2.5
                  functionConstraints binding @?=
                    [ HsConstraint className [TypeVar parameter]
                    , HsConstraint prerequisiteName [TypeVar parameter]
                    ]
                entries -> fail $ "unexpected method projection: " ++ show entries
              length (filter ((== methodName) . functionName)
                $ sourceFunctions projection) @?= 1
              case find ((== ordinaryName) . functionName . sourceBindingFunction)
                  (sourceBindings projection) of
                Just (SourceFunction _) -> pure ()
                entry -> fail $ "ordinary binding acquired class ownership: "
                  ++ show entry
              case Map.lookup className
                  (SharedEnvironment.classDeclarationMap shared) of
                Just (SharedDeclaration.ClassDeclaration _ _ _ _ [method]) -> do
                  SharedDeclaration.valueName method @?=
                    methodName
                  SharedDeclaration.valueAnnotation method @?=
                    SearchPenaltyMetadata (Penalty 2.5)
                  loweredMethod <- expectRight $ fromSynthesisFunctionBinding
                    $ SharedDeclaration.ValueDeclaration method
                  functionConstraints loweredMethod @?=
                    [HsConstraint prerequisiteName [TypeVar parameter]]
                declaration -> fail $ "shared class lost its method: "
                  ++ show declaration
              (SharedDeclaration.valueAnnotation <$> Map.lookup
                  methodName
                  (SharedEnvironment.valueSignatureMap shared)) @?=
                Just (SearchPenaltyMetadata $ Penalty 2.5)
      , testCase "type-synonym phantom parameters use adjacent fresh IDs" $ do
          parsedModule <- expectParsedModule $ unlines
            [ "module Synonyms where"
            , "type Phantom a = Int"
            , "type Flip a b c = Either b a"
            ]
          let declarations = rights
                $ runIdentity $ getTypeDecls [] [parsedModule]
          case declarations of
            [phantom, flipped] -> do
              tdecl_params phantom @?= [0]
              -- RHS occurrence order allocates b then a; the unused c follows
              -- their dense namespace instead of jumping to a sentinel.
              tdecl_params flipped @?= [1, 0, 2]
              Set.size (Set.fromList $ tdecl_params flipped) @?= 3
            result -> fail $ "unexpected synonym declarations: " ++ show result
      , testCase "type declaration errors retain mixed-phase source order" $ do
          parsedModule <- expectParsedModule $ unlines
            [ "module OrderedSynonymErrors where"
            , "type Earlier = Earlier"
            , "type Later = Int ~ Bool"
            ]
          typeNames <- expectRight $ getDataTypes [parsedModule]
          let errors =
                [ failure
                | Left failure <- runIdentity
                    $ getTypeDeclsLocated typeNames [parsedModule]
                ]
          case errors of
            [earlier, later] -> do
              assertBool "earlier semantic error was reordered"
                $ "cyclic type synonym" `isInfixOf`
                    extractionErrorMessage earlier
              assertBool "later raw conversion error was reordered"
                $ "unsupported type syntax" `isInfixOf`
                    extractionErrorMessage later
            _ -> fail $ "unexpected type declaration errors: " ++ show errors
      , testCase "type-synonym foralls shadow their head parameters" $ do
          parsedModule <- expectParsedModule $ unlines
            [ "{-# LANGUAGE RankNTypes #-}"
            , "module SynonymShadow where"
            , "type F a = forall a. a"
            ]
          declarationName <- expectRight
            $ mkQualifiedName ["SynonymShadow"] "F"
          case rights $ runIdentity $ getTypeDecls [] [parsedModule] of
            [declaration] -> declaration @?= HsTypeDecl
              declarationName [0] (TypeForall [1] [] $ TypeVar 1)
            result -> fail $ "unexpected shadowing synonym: " ++ show result
      , testCase "type-synonym heads retain hidden RHS reservations" $ do
          parsedModule <- expectParsedModule $ unlines
            [ "{-# LANGUAGE RankNTypes #-}"
            , "module SynonymHidden where"
            , "type Hidden a phantom = (forall a. a) -> a"
            ]
          declarationName <- expectRight
            $ mkQualifiedName ["SynonymHidden"] "Hidden"
          case rights $ runIdentity $ getTypeDecls [] [parsedModule] of
            [declaration] -> declaration @?= HsTypeDecl
              declarationName [0, 2]
              (TypeArrow
                (TypeForall [1] [] $ TypeVar 1)
                (TypeVar 0))
            result -> fail $ "unexpected hidden-reservation synonym: "
              ++ show result
      , testCase "loader kind-checks phantom alias arguments before erasure" $
          withTemporaryFile (unlines
            [ "module PhantomKind where"
            , "data Higher a = Higher a"
            , "type Phantom a = Int"
            , "bad :: Phantom Higher"
            ]) $ \modulePath -> do
              LoadReport result _ <- environmentFromModule modulePath
              badName <- expectRight
                (mkQualifiedName ["PhantomKind"] "bad")
              case result of
                Left (InvalidSourceInventory
                    (InvalidSourceEnvironmentKinds
                      (SharedKindInference.DeclarationKindError actualName
                        SharedKindInference.KindMismatch{}))) ->
                  actualName @?= badName
                Left failure -> fail $ "unexpected load failure: " ++ show failure
                Right _ -> fail "a phantom alias erased an ill-kinded argument"
      , testCase "compatibility queries check kinds before alias expansion" $
          withTemporaryFile (unlines
            [ "module QueryKinds where"
            , "data Higher a = Higher a"
            , "type Phantom a = Int"
            ]) $ \modulePath -> do
              LoadReport result _ <- environmentFromModule modulePath
              checked <- expectRight result
              let projection = checkedSourceProjection checked
                  inventory = checkedSourceInventory checked
                  parsed = runIdentity $ runExceptT $ parseTypeWithKinds
                    (SharedInventory.inventoryKindAssumptions inventory)
                    (sClassEnv_tclasses $ sourceClasses projection)
                    Nothing
                    (sourceTypeNames projection)
                    (sourceTypeSynonymMap projection)
                    (haskellSrcExtsParseMode "phantom-query")
                    "Phantom Higher"
              case parsed of
                Left failure -> diagnosticCode failure @?= Just "EXF_KIND"
                Right value -> fail $ "ill-kinded query was accepted as "
                  ++ show value
      , testCase "checked projections derive type names from their inventory" $
          withTemporaryFile (unlines
            [ "module TypeNames where"
            , "data Data = MakeData"
            , "type Alias = Data"
            , "class Class a"
            ]) $ \modulePath -> do
              LoadReport parsedResult _ <- parseModules
                [(haskellSrcExtsParseMode modulePath, modulePath)]
              parsed <- expectRight parsedResult
              let poisoned = parsed {sourceTypeNames = [name "Bogus"]}
              checked <- expectRight $ checkSourceEnvironment poisoned
              dataName <- expectRight
                $ mkQualifiedName ["TypeNames"] "Data"
              aliasName <- expectRight
                $ mkQualifiedName ["TypeNames"] "Alias"
              className <- expectRight
                $ mkQualifiedName ["TypeNames"] "Class"
              sourceTypeNames (checkedSourceProjection checked) @?=
                [dataName, aliasName, className]
      , testCase "session queries resolve names and class arities from Inventory" $
          withTemporaryFile (unlines
            [ "module InventoryResolver where"
            , "data Local = Local"
            , "class Pair a b"
            ]) $ \modulePath -> do
              LoadReport parsedResult _ <- parseModules
                [(haskellSrcExtsParseMode modulePath, modulePath)]
              parsed <- expectRight parsedResult
              -- This legacy field is intentionally public for compatibility.
              -- A poisoned value must not cross the checked-session boundary.
              let poisoned = parsed {sourceTypeNames = [name "Bogus"]}
              checked <- expectRight $ checkSourceEnvironment poisoned
              session <- expectRight
                $ ExferenceSession.mkExferenceSession checked
              target <- expectRight $ SharedName.mkIdentifier "answer"
              request <- expectRight $ parseExferenceRequest session
                defaultExferenceOptions target "inventory-resolver-query"
                "Pair Local Local => Local -> Local"
              localName <- expectRight
                $ SharedName.parseName "InventoryResolver.Local"
              pairName <- expectRight
                $ SharedName.parseName "InventoryResolver.Pair"
              case SharedQuery.requestGoal $ exferenceRequestQuery request of
                SharedType.ForallType [] [constraint]
                    (SharedType.FunctionType
                      (SharedType.TypeConstructor parameter)
                      (SharedType.TypeConstructor result)) -> do
                  SharedConstraint.constraintClass constraint @?= pairName
                  SharedConstraint.constraintArguments constraint @?=
                    [ SharedType.TypeConstructor localName
                    , SharedType.TypeConstructor localName
                    ]
                  parameter @?= localName
                  result @?= localName
                actual -> fail $ "unexpected inventory-resolved query: "
                  ++ show actual
              case parseExferenceRequest session defaultExferenceOptions target
                  "inventory-resolver-query" "Pair Local => Local -> Local" of
                Left failure -> do
                  diagnosticCode failure @?= Just "DJEX_EXF_PARSE"
                  assertBool "class arity diagnostic lost its declaration"
                    $ "expected 2, got 1" `isInfixOf`
                        diagnosticMessage failure
                Right requestWithBadArity -> fail
                  $ "inventory class arity was ignored: "
                  ++ show requestWithBadArity
      , testCase "scoped queries enforce interactive module visibility" $
          withTemporaryFile (unlines
            [ "module ScopedFirst where"
            , "data Item = FirstItem"
            ]) $ \firstPath ->
          withTemporaryFile (unlines
            [ "module ScopedSecond where"
            , "data Item = SecondItem"
            ]) $ \secondPath -> do
              ExferenceSessionLoadReport loaded _ <-
                loadExferenceSessionFromFiles [firstPath, secondPath] []
              session <- expectRight loaded
              target <- expectRight $ SharedName.mkIdentifier "answer"
              firstName <- expectRight
                $ SharedName.parseName "ScopedFirst.Item"
              secondName <- expectRight
                $ SharedName.parseName "ScopedSecond.Item"
              firstModule <- expectRight
                $ SharedName.mkModuleName "ScopedFirst"
              aliasModule <- expectRight
                $ SharedName.mkModuleName "Alias"
              reexportModule <- expectRight
                $ SharedName.mkModuleName "Reexport"
              valueOnlyName <- expectRight
                $ SharedName.parseName "ValueOwner.Item"
              let scope current visible aliases = ExferenceQueryScope
                    { exferenceQueryCurrentModule = current
                    , exferenceQueryVisibleNames = visible
                    , exferenceQueryModuleAliases = aliases
                    , exferenceQueryQualifiedNames = []
                    }
                  qualifiedScope admitted = ExferenceQueryScope
                    { exferenceQueryCurrentModule = Nothing
                    , exferenceQueryVisibleNames = []
                    , exferenceQueryModuleAliases = [(aliasModule, firstModule)]
                    , exferenceQueryQualifiedNames = [(aliasModule, admitted)]
                    }
                  parseScoped selectedScope source =
                    parseExferenceRequestInScope
                      session
                      defaultExferenceOptions
                      target
                      selectedScope
                      "scoped-query"
                      source
                  assertGoal label expected selectedScope source = case
                      parseScoped selectedScope source of
                    Left failure -> fail $ label ++ " failed: " ++ show failure
                    Right request -> case
                        SharedQuery.requestGoal
                          $ exferenceRequestQuery request of
                      SharedType.TypeConstructor actual ->
                        assertEqual label expected actual
                      actual -> fail $ label ++ " produced " ++ show actual
                  assertRejected label expectedDetail selectedScope source =
                    case parseScoped selectedScope source of
                      Left failure -> do
                        diagnosticCode failure @?= Just "DJEX_EXF_PARSE"
                        assertBool (label ++ ": " ++ show failure)
                          $ expectedDetail `isInfixOf` diagnosticMessage failure
                      Right request -> fail
                        $ label ++ " accepted " ++ show request
              assertGoal
                "the exact visible unqualified name"
                firstName
                (scope Nothing [firstName] [])
                "Item"
              assertGoal
                "a hidden name's canonical qualifier"
                secondName
                (scope Nothing [firstName] [])
                "ScopedSecond.Item"
              assertGoal
                "an interactive module alias"
                firstName
                (scope Nothing [] [(aliasModule, firstModule)])
                "Alias.Item"
              assertGoal
                "a name selected under a restricted qualifier"
                firstName
                (qualifiedScope [firstName])
                "Alias.Item"
              assertGoal
                "a qualified re-export keeps its defining identity"
                firstName
                ExferenceQueryScope
                  { exferenceQueryCurrentModule = Nothing
                  , exferenceQueryVisibleNames = []
                  , exferenceQueryModuleAliases = []
                  , exferenceQueryQualifiedNames =
                      [(reexportModule, [firstName])]
                  }
                "Reexport.Item"
              assertGoal
                "type lookup ignores a same-spelled admitted value"
                firstName
                ExferenceQueryScope
                  { exferenceQueryCurrentModule = Nothing
                  , exferenceQueryVisibleNames = []
                  , exferenceQueryModuleAliases = []
                  , exferenceQueryQualifiedNames =
                      [(reexportModule, [firstName, valueOnlyName])]
                  }
                "Reexport.Item"
              assertGoal
                "an aliased re-export keeps its defining identity"
                firstName
                ExferenceQueryScope
                  { exferenceQueryCurrentModule = Nothing
                  , exferenceQueryVisibleNames = []
                  , exferenceQueryModuleAliases =
                      [(aliasModule, reexportModule)]
                  , exferenceQueryQualifiedNames =
                      [(aliasModule, [firstName])]
                  }
                "Alias.Item"
              assertRejected
                "a name excluded from a restricted qualifier"
                "is not in scope"
                (qualifiedScope [])
                "Alias.Item"
              assertGoal
                "the current module's local declaration"
                firstName
                (scope (Just firstModule) [secondName, firstName] [])
                "Item"
              assertRejected
                "a loaded but hidden unqualified name"
                "is not in scope"
                (scope Nothing [] [])
                "Item"
              assertRejected
                "two visible unqualified declarations"
                "ambiguous unqualified name"
                (scope Nothing [firstName, secondName] [])
                "Item"
      , testCase "checked inventory retains aliases while projection expands" $
          withTemporaryFile (unlines
            [ "module AliasBoundary where"
            , "type Phantom a = Int"
            , "good :: Phantom Int"
            ]) $ \modulePath -> do
              LoadReport result _ <- environmentFromModule modulePath
              checked <- expectRight result
              phantomName <- expectRight
                (mkQualifiedName ["AliasBoundary"] "Phantom")
              goodName <- expectRight
                (mkQualifiedName ["AliasBoundary"] "good")
              intName <- expectRight $ SharedName.mkIdentifier "Int"
              let inventoryEnvironment = SharedInventory.inventoryEnvironment
                    $ checkedSourceInventory checked
                  projected = sourceFunctions
                    $ checkedSourceProjection checked
              case Map.lookup goodName
                  $ SharedEnvironment.valueSignatureMap inventoryEnvironment of
                Just signature -> assertBool
                  "the checked source signature lost its alias application"
                  $ phantomName `Set.member`
                  SharedType.typeConstructors
                    (SharedDeclaration.valueType signature)
                Nothing -> fail "the checked inventory lost AliasBoundary.good"
              case find ((== goodName) . functionName)
                  projected of
                Just binding -> functionResult binding @?=
                  SharedType.TypeConstructor intName
                Nothing -> fail "the backend projection lost AliasBoundary.good"
      , testCase "post-inventory alias expansion avoids class binder capture" $
          withTemporaryFile (unlines
            [ "{-# LANGUAGE RankNTypes #-}"
            , "module MethodCapture where"
            , "type Ground = forall x. x"
            , "class C a where"
            , "  method :: Ground"
            ]) $ \modulePath -> do
              LoadReport result _ <- environmentFromModule modulePath
              checked <- expectRight result
              className <- expectRight
                $ mkQualifiedName ["MethodCapture"] "C"
              methodName <- expectRight
                $ mkQualifiedName ["MethodCapture"] "method"
              case find ((== methodName) . functionName)
                  $ sourceFunctions $ checkedSourceProjection checked of
                Just FunctionBinding
                    { functionResult = TypeVar methodVariable
                    , functionConstraints =
                        [HsConstraint owner [TypeVar ownerVariable]]
                    } -> do
                      owner @?= className
                      ownerVariable @?= 0
                      methodVariable @?= 1
                Just binding -> fail $ "unexpected method projection: "
                  ++ show binding
                Nothing -> fail "the checked projection lost MethodCapture.method"
      , testCase "checked recursion metadata ignores stale source flags" $ do
          let recursiveName = name "Recursive"
              recursiveConstructor = name "MakeRecursive"
              leafName = name "Leaf"
              leafConstructor = name "MakeLeaf"
              recursiveType = TypeCons recursiveName
              leafType = TypeCons leafName
              environment = SourceEnvironment
                { sourceBindings = map SourceFunction
                    [ FunctionBinding recursiveType recursiveConstructor 0 []
                        [recursiveType]
                    , FunctionBinding leafType leafConstructor 0 [] []
                    ]
                , sourceDeconstructors =
                    [ DeconstructorBinding recursiveType
                        [ConstructorBinding recursiveConstructor [recursiveType]]
                        False
                    , DeconstructorBinding leafType
                        [ConstructorBinding leafConstructor []]
                        True
                    ]
                , sourceClasses = emptyStaticClassEnv
                , sourceTypeNames = [recursiveName, leafName]
                , sourceTypeSynonyms = []
                }
          checked <- case checkSourceEnvironment environment of
            Left failure -> fail $ "unexpected sealing failure: " ++ show failure
            Right value -> pure value
          map deconstructorRecursive
            (sourceDeconstructors $ checkedSourceProjection checked) @?=
              [True, False]
          let declarations = SharedEnvironment.typeDeclarationMap
                $ SharedInventory.inventoryEnvironment
                $ checkedSourceInventory checked
              recursion nameValue = case Map.lookup
                  nameValue declarations of
                Just (SharedDeclaration.DataTypeDeclaration
                    (RecursiveDataMetadata recursive) _ _ _) -> Just recursive
                _ -> Nothing
          map recursion [recursiveName, leafName] @?=
            [Just True, Just False]
      , testCase "checked recursion spans source modules" $
          withTemporaryFile (unlines
            [ "module MutualA where"
            , "import {-# SOURCE #-} MutualB (B)"
            , "data A = MakeA B"
            ]) $ \firstPath ->
          withTemporaryFile (unlines
            [ "module MutualB where"
            , "import MutualA (A)"
            , "data B = MakeB A"
            ]) $ \secondPath -> do
              LoadReport parsedResult _ <- parseModules
                [ (haskellSrcExtsParseMode firstPath, firstPath)
                , (haskellSrcExtsParseMode secondPath, secondPath)
                ]
              parsed <- expectRight parsedResult
              firstName <- expectRight $ mkQualifiedName ["MutualA"] "A"
              secondName <- expectRight $ mkQualifiedName ["MutualB"] "B"
              let mutualFlags environment =
                    [ deconstructorRecursive deconstructor
                    | deconstructor <- sourceDeconstructors environment
                    , typeConstructorHead (deconstructorInput deconstructor)
                        `elem` map Just [firstName, secondName]
                    ]
              -- The compatibility extractor still processes module-local
              -- batches, so this also proves the checked projection no longer
              -- trusts those preliminary bits. The SOURCE edge makes the
              -- mutually recursive declaration graph a valid module graph.
              mutualFlags parsed @?= [False, False]
              checked <- case checkSourceEnvironment parsed of
                Left failure -> fail $ "unexpected sealing failure: "
                  ++ show failure
                Right value -> pure value
              mutualFlags (checkedSourceProjection checked) @?= [True, True]
      , testCase "checked alias recursion shares one prepared witness" $
          withTemporaryFile (unlines
            [ "module AliasRecursion where"
            , "type Self = Loop"
            , "data Loop = MakeLoop Self"
            ]) $ \modulePath -> do
              LoadReport result _ <- environmentFromModule modulePath
              checked <- expectRight result
              loopName <- expectRight
                $ mkQualifiedName ["AliasRecursion"] "Loop"
              let projection = checkedSourceProjection checked
                  annotated = checkedSourceInventory checked
                  neutral = checkedSourcePreparedInventory checked
                  backend = preparedSynthesisBackend neutral
                  declarations = SharedEnvironment.typeDeclarationMap
                    $ SharedInventory.inventoryEnvironment annotated
              SharedTypeSynonym.preparedInventory
                  (preparedSynthesisWitness neutral) @?=
                fmap (const ()) annotated
              environmentFunctions backend @?= sourceFunctions projection
              environmentDeconstructors backend @?=
                sourceDeconstructors projection
              environmentClasses backend @?= sourceClasses projection
              case Map.lookup loopName declarations of
                Just (SharedDeclaration.DataTypeDeclaration
                    (RecursiveDataMetadata recursive) _ _ _) ->
                      recursive @?= True
                declaration -> fail $ "unexpected Loop declaration: "
                  ++ show declaration
              case find ((== Just loopName)
                  . typeConstructorHead . deconstructorInput)
                  (sourceDeconstructors projection) of
                Just deconstructor ->
                  deconstructorRecursive deconstructor @?= True
                Nothing -> fail "the checked projection lost AliasRecursion.Loop"
      , testCase "duplicate synonyms reach the shared inventory in source order" $ do
          parsedModule <- expectParsedModule $ unlines
            [ "module M where"
            , "type Alias = Int"
            , "type Alias = Bool"
            ]
          let results = runIdentity $ getTypeDecls [] [parsedModule]
              synonyms = rights results
          aliasName <- expectRight $ mkQualifiedName ["M"] "Alias"
          map tdecl_name synonyms @?= [aliasName, aliasName]
          let environment :: SourceEnvironment
              environment = SourceEnvironment
                { sourceBindings = []
                , sourceDeconstructors = []
                , sourceClasses = emptyStaticClassEnv
                , sourceTypeNames = [aliasName]
                , sourceTypeSynonyms = synonyms
                }
          toSynthesisSourceInventory environment @?= Left
            (InvalidSharedEnvironment
              $ SharedEnvironment.DuplicateTypeDeclaration
              aliasName)
      , testCase "source inventories require every constructor function" $ do
          let missing = map name ["Just", "Nothing"]
              environment = maybeLikeSourceEnvironment
                { sourceBindings = map SourceFunction $ filter
                    ((`notElem` missing) . functionName)
                    $ sourceFunctions maybeLikeSourceEnvironment
                }
          toSynthesisSourceInventory environment @?= Left
            (MissingConstructorFunctionBindings
              missing)
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
                { sourceBindings = map SourceFunction
                    $ duplicateBindings ++ sourceFunctions maybeLikeSourceEnvironment
                }
          toSynthesisSourceInventory environment @?= Left
            (DuplicateConstructorFunctionBindings
              duplicate)
      , testCase "duplicate tagged methods reach shared validation" $ do
          let className = name "C"
              methodName = name "method"
              classDeclaration = HsTypeClass className [0] []
              method = FunctionBinding
                (TypeVar 0) methodName (Penalty 2.5)
                [classMethodConstraint classDeclaration] [TypeVar 0]
          classes <- expectRight $ mkStaticClassEnv [classDeclaration] []
          let methodBinding = SourceClassMethod className method
              environment = SourceEnvironment
                { sourceBindings = [methodBinding, methodBinding]
                , sourceDeconstructors = []
                , sourceClasses = classes
                , sourceTypeNames = [className]
                , sourceTypeSynonyms = []
                }
          toSynthesisSourceInventory environment @?= Left
            (SynthesisEnvironmentDeclarationError
              $ InvalidSharedDeclaration
              $ SharedDeclaration.DuplicateMethodName methodName)
      , testCase "source inventories reject orphan constructor functions" $ do
          let orphans = map name ["Another", "Orphan"]
              result = TypeApp (TypeCons $ name "Maybe") (TypeVar 0)
              environment = maybeLikeSourceEnvironment
                { sourceBindings = map SourceFunction $
                    [ FunctionBinding result orphan (Penalty 4) [] []
                    | orphan <- orphans
                    ] ++ sourceFunctions maybeLikeSourceEnvironment
                }
          toSynthesisSourceInventory environment @?= Left
            (OrphanConstructorBindings orphans)
      , testCase "source inventories reject mismatched constructor shapes" $ do
          let mismatched = map name ["Just", "Nothing"]
              changeShape binding
                | functionName binding == name "Just" = binding
                    { functionParameters = [] }
                | functionName binding == name "Nothing" = binding
                    { functionParameters = [TypeVar 0] }
                | otherwise = binding
              environment = maybeLikeSourceEnvironment
                { sourceBindings = map SourceFunction $ map changeShape
                    $ sourceFunctions maybeLikeSourceEnvironment
                }
          toSynthesisSourceInventory environment @?= Left
            (MismatchedConstructorFunctionBindings
              mismatched)
      , testCase "constructor reconciliation uses canonical type shapes" $ do
          let variable = TypeVar 0
              constructorApplication constructor arguments = foldl TypeApp
                (TypeCons constructor) arguments
              spellings =
                [ ( TypeArrow variable variable
                  , constructorApplication SharedName.functionName
                      [variable, variable]
                  )
                , ( TypeTuple Boxed [variable, variable]
                  , constructorApplication (validTupleName 2)
                      [variable, variable]
                  )
                ]
              justName = name "Just"
              withField field environment = environment
                { sourceDeconstructors =
                    [ deconstructor
                        { deconstructorConstructors =
                            [ if constructorName constructor == justName
                                then constructor { constructorFields = [field] }
                                else constructor
                            | constructor <- deconstructorConstructors deconstructor
                            ]
                        }
                    | deconstructor <- sourceDeconstructors environment
                    ]
                }
              withParameter parameter environment = environment
                { sourceBindings =
                    [ case binding of
                        SourceFunction function
                          | functionName function == justName ->
                              SourceFunction function
                                { functionParameters = [parameter] }
                        _ -> binding
                    | binding <- sourceBindings environment
                    ]
                }
          forM_ spellings $ \(structural, constructorBacked) -> do
            let environment = withParameter constructorBacked
                  $ withField structural maybeLikeSourceEnvironment
            _ <- expectRight $ toSynthesisSourceInventory environment
            pure ()
      , testCase "source inventories retain constructor and value penalties" $ do
          inventory <- expectRight
            $ toSynthesisSourceInventory maybeLikeSourceEnvironment
          let shared = SharedInventory.inventoryEnvironment inventory
              constructors = SharedEnvironment.dataConstructorMap shared
              values = SharedEnvironment.valueSignatureMap shared
              constructorPenalty constructor =
                SharedDeclaration.constructorAnnotation
                  <$> Map.lookup (name constructor)
                    constructors
              valuePenalty value =
                SharedDeclaration.valueAnnotation
                  <$> Map.lookup (name value) values
          constructorPenalty "Nothing" @?=
            Just (SearchPenaltyMetadata $ Penalty 1.25)
          constructorPenalty "Just" @?=
            Just (SearchPenaltyMetadata $ Penalty 2.75)
          valuePenalty "defaultMaybe" @?=
            Just (SearchPenaltyMetadata $ Penalty 3.5)
      , testCase "the shipped source environment seals as one inventory" $ do
          environmentDirectory <- getDataFileName "exference/environment"
          (sourceEnvironmentResult, _) <- runLoad
            $ environmentFromPath environmentDirectory
          checkedEnvironment <- expectRight sourceEnvironmentResult
          let inventory = checkedSourceInventory checkedEnvironment
              shared = SharedInventory.inventoryEnvironment inventory
          assertBool "shared source inventory lost type declarations"
            $ not $ Map.null $ SharedEnvironment.typeDeclarationMap shared
          assertBool "shared source inventory lost values"
            $ not $ Map.null $ SharedEnvironment.valueSignatureMap shared
          maybeName <- expectRight
            $ mkQualifiedName ["Data", "Maybe"] "Maybe"
          Map.lookup maybeName
              (SharedKindInference.typeConstructorKinds
                $ SharedInventory.inventoryKindAssumptions inventory) @?=
            Just (SharedKind.FunctionKind
              SharedKind.ProperTypeKind SharedKind.ProperTypeKind)
      , testCase "the shipped catalogue distinguishes opaque and empty types" $ do
          environmentDirectory <- getDataFileName "exference/environment"
          (sourceEnvironmentResult, _) <- runLoad
            $ environmentFromPath environmentDirectory
          checkedEnvironment <- expectRight sourceEnvironmentResult
          intName <- expectRight $ mkQualifiedName ["Data", "Int"] "Int"
          mapName <- expectRight $ mkQualifiedName ["Data", "Map"] "Map"
          altName <- expectRight $ mkQualifiedName ["Data", "Monoid"] "Alt"
          voidName <- expectRight $ mkQualifiedName ["Data", "Void"] "Void"
          v1Name <- expectRight $ mkQualifiedName ["GHC", "Generics"] "V1"
          rec1Name <- expectRight $ mkQualifiedName ["GHC", "Generics"] "Rec1"
          m1Name <- expectRight $ mkQualifiedName ["GHC", "Generics"] "M1"
          let shared = SharedInventory.inventoryEnvironment
                $ checkedSourceInventory checkedEnvironment
              declarations = SharedEnvironment.typeDeclarationMap shared
              emptyDeclarationNames = Set.fromList
                [ typeName
                | SharedDeclaration.DataTypeDeclaration
                    _ typeName _ [] <- Map.elems declarations
                ]
              projection = checkedSourceProjection checkedEnvironment
              deconstructorNames =
                [ dataName
                | deconstructor <- sourceDeconstructors projection
                , Just dataName <-
                    [typeConstructorHead $ deconstructorInput deconstructor]
                ]
              assertAbstract expectedName expectedKind =
                case Map.lookup expectedName declarations of
                  Just (SharedDeclaration.AbstractTypeDeclaration
                      _ actualName actualKind) -> do
                    actualName @?= expectedName
                    actualKind @?= expectedKind
                  declaration -> fail $ "expected abstract declaration for "
                    ++ show expectedName ++ ", got " ++ show declaration
              assertEmpty expectedName =
                case Map.lookup expectedName declarations of
                  Just (SharedDeclaration.DataTypeDeclaration
                      _ actualName _ constructors) -> do
                    actualName @?= expectedName
                    constructors @?= []
                  declaration -> fail $ "expected empty declaration for "
                    ++ show expectedName ++ ", got " ++ show declaration
              proper = SharedKind.ProperTypeKind
              unary = SharedKind.FunctionKind proper proper
              unaryWrapper = SharedKind.FunctionKind unary
                $ SharedKind.FunctionKind proper proper
              metadataWrapper = SharedKind.FunctionKind proper
                $ SharedKind.FunctionKind proper
                $ SharedKind.FunctionKind unary
                $ SharedKind.FunctionKind proper proper
          assertAbstract intName proper
          assertAbstract mapName
            $ SharedKind.FunctionKind proper
            $ SharedKind.FunctionKind proper proper
          assertAbstract altName unaryWrapper
          assertAbstract rec1Name unaryWrapper
          assertAbstract m1Name metadataWrapper
          assertEmpty voidName
          assertEmpty v1Name
          emptyDeclarationNames @?= Set.fromList [voidName, v1Name]
          assertBool "opaque Int retained an empty-case deconstructor"
            $ intName `notElem` deconstructorNames
          assertBool "opaque Map retained an empty-case deconstructor"
            $ mapName `notElem` deconstructorNames
          assertBool "real Void lost its empty-case deconstructor"
            $ voidName `elem` deconstructorNames
          assertBool "real V1 lost its empty-case deconstructor"
            $ v1Name `elem` deconstructorNames
      , testCase "visibility sidecars are path-local and preserve real empties" $ do
          let moduleSource = unlines
                [ "{-# LANGUAGE EmptyDataDecls #-}"
                , "{-# LANGUAGE MagicHash #-}"
                , "{-# LANGUAGE TypeOperators #-}"
                , "module Fixture where"
                , "data Token a"
                , "data Empty"
                , "data left :# right"
                ]
              visibilitySource = unlines
                [ "abstract Fixture.Token 1 Type"
                , "empty Fixture.Empty 0"
                , "abstract Fixture.(:#) 2 Type Type"
                ]
          withTemporaryDirectoryFiles
              [("Fixture.hs", moduleSource)] $ \plainDirectory -> do
            (plainResult, _) <- runLoad
              $ environmentFromPath plainDirectory
            plain <- checkedSourceProjection <$> expectRight plainResult
            tokenName <- expectRight $ mkQualifiedName ["Fixture"] "Token"
            assertBool "a source-only empty declaration became abstract"
              $ tokenName `elem`
                [ dataName
                | deconstructor <- sourceDeconstructors plain
                , Just dataName <-
                    [typeConstructorHead $ deconstructorInput deconstructor]
                ]
          LoadReport legacySnapshotResult _ <- environmentFromSources
            [("Fixture.hs", moduleSource)] []
          legacySnapshot <- checkedSourceProjection
            <$> expectRight legacySnapshotResult
          legacyTokenName <- expectRight
            $ mkQualifiedName ["Fixture"] "Token"
          assertBool "the legacy snapshot API stopped treating constructorless data normally"
            $ legacyTokenName `elem`
              [ dataName
              | deconstructor <- sourceDeconstructors legacySnapshot
              , Just dataName <-
                  [typeConstructorHead $ deconstructorInput deconstructor]
              ]
          withTemporaryDirectoryFiles
              [ ("Fixture.hs", moduleSource)
              , ("types.visibility", visibilitySource)
              ] $ \classifiedDirectory -> do
            (classifiedResult, _) <- runLoad
              $ environmentFromPath classifiedDirectory
            checked <- expectRight classifiedResult
            tokenName <- expectRight $ mkQualifiedName ["Fixture"] "Token"
            emptyName <- expectRight $ mkQualifiedName ["Fixture"] "Empty"
            operatorName <- expectRight $ mkQualifiedName ["Fixture"] "(:#)"
            let projection = checkedSourceProjection checked
                deconstructorNames =
                  [ dataName
                  | deconstructor <- sourceDeconstructors projection
                  , Just dataName <-
                      [typeConstructorHead $ deconstructorInput deconstructor]
                  ]
                declarations = SharedEnvironment.typeDeclarationMap
                  $ SharedInventory.inventoryEnvironment
                  $ checkedSourceInventory checked
            assertBool "abstract Token retained an eliminator"
              $ tokenName `notElem` deconstructorNames
            assertBool "a hash type operator was truncated as a comment"
              $ operatorName `notElem` deconstructorNames
            assertBool "explicitly empty Empty lost its eliminator"
              $ emptyName `elem` deconstructorNames
            case Map.lookup tokenName declarations of
              Just SharedDeclaration.AbstractTypeDeclaration{} -> pure ()
              declaration -> fail $ "Token was not abstract: "
                ++ show declaration
            case Map.lookup operatorName declarations of
              Just SharedDeclaration.AbstractTypeDeclaration{} -> pure ()
              declaration -> fail $ "(:#) was not abstract: "
                ++ show declaration
            case Map.lookup emptyName declarations of
              Just SharedDeclaration.DataTypeDeclaration{} -> pure ()
              declaration -> fail $ "Empty was not concrete: "
                ++ show declaration
            LoadReport snapshotResult _ <-
              environmentFromSourcesWithTypeVisibility
                [("Fixture.hs", moduleSource)] []
                [("types.visibility", visibilitySource)]
            snapshot <- expectRight snapshotResult
            let snapshotDeclarations = SharedEnvironment.typeDeclarationMap
                  $ SharedInventory.inventoryEnvironment
                  $ checkedSourceInventory snapshot
                snapshotDeconstructors =
                  [ dataName
                  | deconstructor <- sourceDeconstructors
                      $ checkedSourceProjection snapshot
                  , Just dataName <-
                      [typeConstructorHead $ deconstructorInput deconstructor]
                  ]
            case Map.lookup tokenName snapshotDeclarations of
              Just SharedDeclaration.AbstractTypeDeclaration{} -> pure ()
              declaration -> fail $ "snapshot Token was not abstract: "
                ++ show declaration
            assertBool "snapshot abstract Token retained an eliminator"
              $ tokenName `notElem` snapshotDeconstructors
            assertBool "snapshot empty Empty lost its eliminator"
              $ emptyName `elem` snapshotDeconstructors
      , testCase "visibility manifests reject every catalogue mismatch" $ do
          let moduleSource = unlines
                [ "{-# LANGUAGE EmptyDataDecls #-}"
                , "module Fixture where"
                , "data Token a"
                , "data Empty"
                , "data Full = Full"
                ]
              cases =
                [ ( "classification"
                  , [ "opaque Fixture.Token 1 Type"
                    , "empty Fixture.Empty 0"
                    ]
                  , "unknown classification"
                  )
                , ( "unqualified"
                  , [ "abstract Token 1 Type"
                    , "empty Fixture.Empty 0"
                    ]
                  , "must be module-qualified"
                  )
                , ( "malformed arity"
                  , [ "abstract Fixture.Token nope Type"
                    , "empty Fixture.Empty 0"
                    ]
                  , "invalid nonnegative type arity"
                  )
                , ( "kind count"
                  , [ "abstract Fixture.Token 1"
                    , "empty Fixture.Empty 0"
                    ]
                  , "requires 1 parameter kind, but found 0"
                  )
                , ( "invalid kind"
                  , [ "abstract Fixture.Token 1 Type->Type"
                    , "empty Fixture.Empty 0"
                    ]
                  , "invalid parameter kind"
                  )
                , ( "duplicate"
                  , [ "abstract Fixture.Token 1 Type"
                    , "empty Fixture.Empty 0"
                    , "abstract Fixture.Token 1 Type"
                    ]
                  , "duplicate classification for Fixture.Token"
                  )
                , ( "unknown"
                  , [ "abstract Fixture.Token 1 Type"
                    , "empty Fixture.Empty 0"
                    , "abstract Fixture.Missing 0"
                    ]
                  , "unknown datatype Fixture.Missing"
                  )
                , ( "nonempty"
                  , [ "abstract Fixture.Token 1 Type"
                    , "empty Fixture.Empty 0"
                    , "abstract Fixture.Full 0"
                    ]
                  , "has constructors and cannot be classified as abstract"
                  )
                , ( "arity"
                  , [ "abstract Fixture.Token 2 Type Type"
                    , "empty Fixture.Empty 0"
                    ]
                  , "arity mismatch for Fixture.Token"
                  )
                , ( "incomplete"
                  , ["abstract Fixture.Token 1 Type"]
                  , "missing classification for Fixture.Empty"
                  )
                ]
          forM_ cases $ \(label, manifestLines, expectedMessage) ->
            withTemporaryDirectoryFiles
                [ ("Fixture.hs", moduleSource)
                , ("types.visibility", unlines manifestLines)
                ] $ \environmentDirectory -> do
              (result, _) <- runLoad
                $ environmentFromPath environmentDirectory
              failures <- case result of
                Left (TypeVisibilityManifestErrors values) -> pure values
                Left failure -> fail $ label ++ " produced the wrong failure: "
                  ++ show failure
                Right _ -> fail $ label ++ " manifest was accepted"
              let rendered = environmentLoadErrorDiagnostics
                    $ TypeVisibilityManifestErrors failures
              assertBool (label ++ " diagnostic omitted its reason")
                $ any (isInfixOf expectedMessage . diagnosticMessage)
                $ NonEmpty.toList rendered
              case label of
                "incomplete" -> pure ()
                _ -> assertBool
                  (label ++ " manifest diagnostic lost its line span")
                  $ any ((/= Nothing) . diagnosticSpan)
                  $ NonEmpty.toList rendered
              assertBool (label ++ " diagnostic lost its stable code")
                $ all ((== Just "EXF_TYPE_VISIBILITY") . diagnosticCode)
                $ NonEmpty.toList rendered
      , testCase "built-in constructors retain configured search penalties" $ do
          environmentDirectory <- getDataFileName "exference/environment"
          (sourceEnvironmentResult, _) <- runLoad
            $ environmentFromPath environmentDirectory
          checkedEnvironment <- expectRight sourceEnvironmentResult
          let constructors = SharedEnvironment.dataConstructorMap
                $ SharedInventory.inventoryEnvironment
                $ checkedSourceInventory checkedEnvironment
              constructorPenalty constructor =
                SharedDeclaration.constructorAnnotation
                  <$> Map.lookup constructor constructors
          mapM_ (\(constructor, expectedPenalty) ->
              constructorPenalty constructor @?=
                Just (SearchPenaltyMetadata expectedPenalty))
            [ (ListCon, Penalty 0)
            , (Cons, Penalty 5)
            , (validTupleName 0, Penalty 9.9)
            , (validTupleName 2, Penalty 5)
            , (validTupleName 3, Penalty 5)
            , (validTupleName 4, Penalty 4)
            , (validTupleName 5, Penalty 3)
            , (validTupleName 6, Penalty 2)
            , (validTupleName 7, Penalty 0)
            ]
      , testCase "source environments reject ill-kinded signatures" $ do
          let intName = name "Int"
              badName = name "bad"
              environment = SourceEnvironment
                { sourceBindings =
                    [ SourceFunction $ FunctionBinding
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
                badName
                (SharedKindInference.KindMismatch proper
                  $ SharedKind.FunctionKind proper proper)))
      , testCase "loader names unknown type constructors precisely" $ do
          withTemporaryFile (unlines
            [ "module Warnings where"
            , "external :: Data.External.External"
            ]) $ \modulePath -> do
              (result, messages) <- runLoad
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
              (result, messages) <- runLoad
                $ parseModules
                    [(haskellSrcExtsParseMode modulePath, modulePath)]
              _ <- expectRight result
              assertBool
                ("missing constraint-class diagnostic: " ++ show messages)
                $ "unknown constraint class 'External.Constraint' used in the binding Warnings.constrained"
                    `elem` messages
      , testCase "loader accepts infix classes at every constraint site" $ do
          withTemporaryFile (unlines
            [ "module OperatorClasses where"
            , "class left :== right"
            , "class (left :== right) => Child left right"
            , "instance (left :== right) => Child left right"
            , "constrained :: (left :== right) => left -> right"
            ]) $ \modulePath -> do
              (result, _) <- runLoad $ parseModules
                [(haskellSrcExtsParseMode modulePath, modulePath)]
              _ <- expectRight result
              pure ()
      , testCase "loader retains constraints nested in constraint arguments" $ do
          withTemporaryFile (unlines
            [ "module Warnings where"
            , "class Outer a"
            , "nested :: Outer (forall b. External.Constraint b => b) => Int"
            ]) $ \modulePath -> do
              let baseMode = haskellSrcExtsParseMode modulePath
                  rankNMode = baseMode
                    { HSE.extensions = HSE.EnableExtension HSE.RankNTypes
                        : HSE.extensions baseMode
                    }
              (result, messages) <- runLoad
                $ parseModules [(rankNMode, modulePath)]
              _ <- expectRight result
              assertBool
                ("missing nested constraint-class diagnostic: "
                  ++ show messages)
                $ "unknown constraint class 'External.Constraint' used in the binding Warnings.nested"
                    `elem` messages
      , testCase "partial class inventories fail before advisory warnings" $ do
          environmentDirectory <- getDataFileName "exference/environment"
          let paths = map ((environmentDirectory ++ "/") ++)
                ["Eq.hs", "Ord.hs"]
              warning = "unknown type constructor 'Data.Monoid.Last' used in class instances"
          (result, messages) <- runLoad $ parseModules
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
            @?= Right (validQualifiedName ["Control", "Applicative"] "<*>")
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
                [ validQualifiedName [] "."
                , validQualifiedName ["Control", "Applicative"] "<*>"
                , validQualifiedName ["Control", "Monad"] ">>="
                , validQualifiedName [] "⊕"
                , validQualifiedName ["Math", "Operators"] "⊕"
                , validQualifiedName [] "->"
                , ListCon
                , validTupleName 0
                , Cons
                ] ++ map validTupleName [2 .. 7]
          show (validQualifiedName ["Control", "Applicative"] "<*>")
            @?= "Control.Applicative.(<*>)"
          show (validQualifiedName ["Math", "Operators"] "⊕")
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
            , validTupleName 0
            , Cons
            , validTupleName 2
            , validTupleName 3
            , validQualifiedName [] "->"
            ]
      , testCase "the shipped rating file parses and every name round-trips" $ do
          environmentDirectory <- getDataFileName "exference/environment"
          contents <- readFile $ environmentDirectory ++ "/all.ratings"
          ratings <- expectRight $ parseRatings contents
          assertBool "the shipped rating file unexpectedly parsed no entries"
            (not $ null ratings)
          mapM_ (\(qualifiedName, _) ->
              parseQualifiedName (show qualifiedName) @?= Right qualifiedName)
            ratings
      , testCase "the built-in unit value inhabits parsed unit" $ do
          environmentDirectory <- getDataFileName "exference/environment"
          (bindings, classEnvironment, messages) <-
            loadEnvironmentAndMessages environmentDirectory
          let unitBindings = filter ((== validTupleName 0) . functionName) bindings
              applicativeOperator = validQualifiedName
                ["Control", "Applicative"] "<*>"
          case find ((== applicativeOperator) . functionName) bindings of
            Nothing -> fail "the shipped Applicative operator was not loaded"
            Just binding -> functionPenalty binding @?= Penalty 0.3
          mapM_ (\(constructor, expectedPenalty) ->
              case find ((== constructor) . functionName) bindings of
                Nothing -> fail $ "built-in constructor was not loaded: "
                  ++ show constructor
                Just binding -> functionPenalty binding @?= expectedPenalty)
            [ (validTupleName 0, Penalty 9.9)
            , (Cons, Penalty 5.0)
            , (validTupleName 2, Penalty 5.0)
            , (validTupleName 3, Penalty 5.0)
            , (validTupleName 4, Penalty 4.0)
            , (validTupleName 5, Penalty 3.0)
            , (validTupleName 6, Penalty 2.0)
            , (validTupleName 7, Penalty 0)
            ]
          assertBool "the shipped class table is empty"
            (not $ Map.null $ sClassEnv_tclasses classEnvironment)
          assertBool "the shipped instance index is empty"
            (not $ Map.null $ sClassEnv_instances classEnvironment)
          Map.size (sClassEnv_tclasses classEnvironment) @?= 41
          sum (map length $ Map.elems $ sClassEnv_instances classEnvironment)
            @?= 432
          messages @?=
              [ "got 41 classes"
              , "and 432 instances"
            , "(-> 432 superclass-completed resolution rules)"
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
      , testCase "tuple names retain the larger shared representational limit" $ do
          mkBoxedTupleName SharedName.maximumTupleArity @?=
            Right (validTupleName SharedName.maximumTupleArity)
          case mkBoxedTupleName (SharedName.maximumTupleArity + 1) of
            Left _ -> pure ()
            Right result -> fail $ "over-limit tuple was accepted as "
              ++ show result
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
                , diagnosticSpan = Just span'
                } ->
                  let start = sourceStart span'
                  in (sourceLine start > 0 && sourceColumn start > 0) @?= True
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
      , testCase "infix class constraints match their prefix form" $ do
          infixResult <- expectRight
            $ parseTypePure "(left :== right) => left -> right"
          prefixResult <- expectRight
            $ parseTypePure "((:==) left right) => left -> right"
          infixResult @?= prefixResult
          case infixResult of
            ( TypeForall []
                [HsConstraint className [TypeVar 0, TypeVar 1]]
                (TypeArrow (TypeVar 0) (TypeVar 1))
              , hints
              ) -> do
                className @?= name ":=="
                hints @?= Map.fromList [("left", 0), ("right", 1)]
            result -> fail $ "unexpected infix constraint: " ++ show result
      , testCase "infix type operators share prefix elaboration and aliases" $ do
          let operatorName = validQualifiedName ["TypeOwner"] ":*:"
              baseResolver = legacyTypeResolver Map.empty [operatorName]
              unqualifiedResolver = scopeTypeResolverWithQualifiedNames
                [operatorName] [] [] baseResolver
          (infixType, infixHints) <- expectRight
            $ parseTypeWithTestResolver unqualifiedResolver
                "left :*: right"
          (prefixType, prefixHints) <- expectRight
            $ parseTypeWithTestResolver unqualifiedResolver
                "(:*:) left right"
          infixType @?= TypeApp
            (TypeApp (TypeCons operatorName) (TypeVar 0))
            (TypeVar 1)
          infixHints @?= Map.fromList [("left", 0), ("right", 1)]
          (infixType, infixHints) @?= (prefixType, prefixHints)

          ownerModule <- expectRight $ SharedName.mkModuleName "TypeOwner"
          aliasModule <- expectRight $ SharedName.mkModuleName "Alias"
          let aliasedResolver = scopeTypeResolverWithQualifiedNames
                []
                [(aliasModule, ownerModule)]
                [(aliasModule, [operatorName])]
                baseResolver
          aliased <- expectRight $ parseTypeWithTestResolver aliasedResolver
            "left Alias.:*: right"
          aliased @?= (infixType, infixHints)
      , testCase "infix type operators obey scope and ambiguity checks" $ do
          let firstOperator = validQualifiedName ["First"] ":*:"
              secondOperator = validQualifiedName ["Second"] ":*:"
              baseResolver = legacyTypeResolver
                Map.empty [firstOperator, secondOperator]
              hiddenResolver = scopeTypeResolverWithQualifiedNames
                [] [] [] baseResolver
              ambiguousResolver = scopeTypeResolverWithQualifiedNames
                [firstOperator, secondOperator] [] [] baseResolver
              assertRejected label detail resolver =
                case parseTypeWithTestResolver resolver "left :*: right" of
                  Left message -> assertBool (label ++ ": " ++ message)
                    $ detail `isInfixOf` message
                  Right converted -> fail $ label ++ " accepted "
                    ++ show converted
          assertRejected
            "a hidden loaded operator"
            "is not in scope"
            hiddenResolver
          assertRejected
            "two visible operators"
            "ambiguous unqualified name"
            ambiguousResolver

          firstModule <- expectRight $ SharedName.mkModuleName "First"
          aliasModule <- expectRight $ SharedName.mkModuleName "Alias"
          let excludedAliasResolver = scopeTypeResolverWithQualifiedNames
                []
                [(aliasModule, firstModule)]
                [(aliasModule, [])]
                baseResolver
          case parseTypeWithTestResolver excludedAliasResolver
              "left Alias.:*: right" of
            Left message -> assertBool message
              $ "is not in scope" `isInfixOf` message
            Right converted -> fail $ "excluded qualified operator accepted "
              ++ show converted
      , testCase "explicit forall retains its source hint identity" $ do
          (converted, hints) <- expectRight
            $ parseTypePure "forall foo. foo -> foo"
          converted @?= TypeForall [0] []
            (TypeArrow (TypeVar 0) (TypeVar 0))
          hints @?= Map.singleton "foo" 0
      , testCase "rank-N forall does not capture a later free spelling" $ do
          (converted, hints) <- expectRight
            $ parseTypePure "(forall foo. foo -> foo) -> foo"
          converted @?= TypeArrow
            (TypeForall [1] [] $ TypeArrow (TypeVar 1) (TypeVar 1))
            (TypeVar 0)
          hints @?= Map.singleton "foo" 0
      , testCase "nested forall contexts follow lexical shadowing" $ do
          (converted, hints) <- expectRight $ parseTypePure
            "forall a. Outer a => forall a. Inner a => a -> a"
          converted @?= TypeForall [0]
            [HsConstraint (name "Outer") [TypeVar 0]]
            (TypeForall [1]
              [HsConstraint (name "Inner") [TypeVar 1]]
              (TypeArrow (TypeVar 1) (TypeVar 1)))
          hints @?= Map.singleton "a" 0
      ]
  , testGroup "source type-variable hints"
      [ testCase "accept every flexible position and Int boundary" $ do
          let ground = TypeCons $ name "Ground"
              fixtures =
                [ ( "free occurrence"
                  , TypeVar 7
                  , Map.singleton "free" 7
                  )
                , ( "forall binder"
                  , TypeForall [11] [] ground
                  , Map.singleton "quantified" 11
                  )
                , ( "forall context"
                  , TypeForall []
                      [HsConstraint (name "C") [TypeVar 13]] ground
                  , Map.singleton "contextual" 13
                  )
                , ( "minimum Int"
                  , TypeVar minBound
                  , Map.singleton "minimum" minBound
                  )
                , ( "maximum Int"
                  , TypeVar maxBound
                  , Map.singleton "maximum" maxBound
                  )
                ]
          mapM_ (\(label, goal, sourceNames) ->
              case mkExferenceSourceTypeVariableHints goal sourceNames of
                Left failure -> fail $ label ++ " hint was rejected: "
                  ++ show failure
                Right hints -> force hints @?= hints)
            fixtures
          emptyHints <- expectRight
            $ mkExferenceSourceTypeVariableHints ground Map.empty
          emptyHints @?= emptyExferenceSourceTypeVariableHints ground
      , testCase "reject every non-variable source spelling exactly" $ do
          let goal = TypeVar 0
              invalid spelling failure =
                mkExferenceSourceTypeVariableHints goal
                  (Map.singleton spelling 0) @?= Left failure
          invalid "_" WildcardSourceTypeVariableSpelling
          mapM_ (uncurry invalid)
            [ ("", InvalidSourceTypeVariableSpelling "" SharedName.EmptyName)
            , ("A", InvalidSourceTypeVariableSpelling "A"
                $ SharedName.InvalidIdentifier "A")
            , ("where", InvalidSourceTypeVariableSpelling "where"
                $ SharedName.ReservedIdentifier "where")
            , ("forall", InvalidSourceTypeVariableSpelling "forall"
                $ SharedName.ReservedIdentifier "forall")
            , ("family", InvalidSourceTypeVariableSpelling "family"
                $ SharedName.ReservedIdentifier "family")
            , ("a b", InvalidSourceTypeVariableSpelling "a b"
                $ SharedName.InvalidIdentifier "a b")
            , ("a\nb", InvalidSourceTypeVariableSpelling "a\nb"
                $ SharedName.InvalidIdentifier "a\nb")
            , ("\ESC[31m", InvalidSourceTypeVariableSpelling "\ESC[31m"
                $ SharedName.InvalidIdentifier "\ESC[31m")
            , ("+", InvalidSourceTypeVariableSpelling "+"
                $ SharedName.InvalidIdentifier "+")
            ]
      , testCase "reject hints outside the flexible source scope" $ do
          mkExferenceSourceTypeVariableHints (TypeVar 0)
              (Map.singleton "outside" 1) @?=
            Left (SourceTypeVariableHintOutOfScope "outside" 1)
          mkExferenceSourceTypeVariableHints (TypeConstant 7)
              (Map.singleton "rigid" 7) @?=
            Left (SourceTypeVariableHintOutOfScope "rigid" 7)
      , testCase "validate every alias before deterministic collapse" $ do
          let goal = TypeArrow (TypeVar 7) (TypeVar 7)
              sourceNames = Map.fromList [("zeta", 7), ("alpha", 7)]
          mkExferenceSourceTypeVariableHints goal
              (Map.fromList [("alpha", 7), ("where", 7)]) @?=
            Left (InvalidSourceTypeVariableSpelling "where"
              $ SharedName.ReservedIdentifier "where")
          aliases <- expectRight $ mkExferenceSourceTypeVariableHints goal
            sourceNames
          preferred <- expectRight $ mkExferenceSourceTypeVariableHints goal
            $ Map.singleton "alpha" 7
          aliases @?= preferred
          showHsType sourceNames (TypeVar 7) @?= "alpha"
          targetName <- expectRight $ SharedName.mkIdentifier "hinted"
          target <- expectRight $ Generated.mkDefinitionName targetName
          let input = identityInput {input_goalType = goal}
          environment <- expectRight $ sealLegacyEnvironment input
          results <- expectRight $ findQueryResultsInEnvironmentEither
            target aliases environment (legacyInputQuery input)
          case concatMap
              (SharedSearch.batchCandidates . SharedQuery.resultSearch)
              results of
            candidate : _ -> exferenceTypeVariableHints
                (SharedCandidate.candidateDetails candidate) @?=
              Map.fromList
                [ (SharedType.FlexibleVariable 7, "alpha")
                , (SharedType.RigidVariable 0, "alpha")
                ]
            [] -> fail "canonical source-hint search found no identity"
      , testCase "reject reuse against a different same-ID query" $ do
          let hintedGoal = TypeVar 0
              searchedGoal = TypeArrow (TypeVar 0) (TypeVar 0)
              input = identityInput {input_goalType = searchedGoal}
          hints <- expectRight $ mkExferenceSourceTypeVariableHints
            hintedGoal $ Map.singleton "fromOtherQuery" 0
          targetName <- expectRight $ SharedName.mkIdentifier "hinted"
          target <- expectRight $ Generated.mkDefinitionName targetName
          environment <- expectRight $ sealLegacyEnvironment input
          findQueryResultsInEnvironmentEither target hints environment
              (legacyInputQuery input) @?=
            Left (InvalidSourceTypeVariableHints
              $ SourceTypeVariableHintGoalMismatch
                  hintedGoal searchedGoal)
      , testCase "detach accepted and rejected spellings eagerly" $ do
          let partialSpelling = 'a' : error "unforced hint spelling tail"
              sourceNames = Map.singleton partialSpelling 0
          result <- try $ evaluate
            $ mkExferenceSourceTypeVariableHints (TypeVar 0) sourceNames
          case result
              :: Either SomeException
                  (Either
                    ExferenceSourceTypeVariableHintError
                    ExferenceSourceTypeVariableHints) of
            Left _ -> pure ()
            Right _ -> fail
              "checked source hints retained a lazy spelling validation"
          let partialInvalid = '\ESC'
                : error "unforced invalid hint spelling tail"
          invalidResult <- try $ evaluate
            $ mkExferenceSourceTypeVariableHints (TypeVar 0)
            $ Map.singleton partialInvalid 0
          case invalidResult
              :: Either SomeException
                  (Either
                    ExferenceSourceTypeVariableHintError
                    ExferenceSourceTypeVariableHints) of
            Left _ -> pure ()
            Right _ -> fail
              "rejected source hints retained a lazy spelling tail"
      ]
  , testGroup "shared generated output"
      [ testCase "typed expressions erase to stable local identities" $ do
          let variable = TypeVar 0
              expression = ExpLambda 1 variable
                $ ExpApply (ExpName $ name "id") (ExpVar 1 variable)
          toGeneratedExpression expression @?=
            Generated.Lambda [Generated.Bind 1]
              (Generated.Apply
                (Generated.Global $ name "id")
                (Generated.Local 1))
      , testCase "typed compatibility patterns share the complete generated shape" $ do
          let integer = TypeCons $ name "Int"
              boolean = TypeCons $ name "Bool"
              functionType = TypeArrow boolean integer
              box = name "Box"
              just = name "Just"
              source = name "source"
              expression = ExpLetMatch box [(1, integer)] (ExpName source)
                $ ExpCaseMatch (ExpVar 1 integer)
                    [ ( just
                      , [(2, boolean)]
                      , ExpLet 3 functionType
                          (ExpLambda 4 boolean $ ExpVar 1 integer)
                          (ExpApply
                            (ExpVar 3 functionType)
                            (ExpVar 2 boolean))
                      )
                    ]
              generated = Generated.Let
                (Generated.Constructor box
                  [Generated.Bind 1])
                (Generated.Global source)
                (Generated.Case (Generated.Local 1)
                  [ ( Generated.Constructor just
                        [Generated.Bind 2]
                    , Generated.Let (Generated.Bind 3)
                        (Generated.Lambda [Generated.Bind 4]
                          $ Generated.Local 1)
                        (Generated.Apply
                          (Generated.Local 3)
                          (Generated.Local 2))
                    )
                  ])
          toGeneratedExpression expression @?= generated
          expressionTypedLocals expression @?=
            [ (1, integer)
            , (1, integer)
            , (2, boolean)
            , (3, functionType)
            , (4, boolean)
            , (1, integer)
            , (3, functionType)
            , (2, boolean)
            ]
          expressionNameHints expression @?= Map.fromList
            [(1, "i1"), (2, "b2"), (3, "f3"), (4, "b4")]
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
      , testCase "derived list hints avoid reserved local identifiers" $ do
          let listType = TypeApp
                (TypeCons SharedName.listName) (TypeVar 0)
              expression = ExpLambda 1 listType $ ExpVar 1 listType
          -- The old pluralization produced @as@ for local ID 1. Derived
          -- preferences enter the shared renderer as authoritative hints, so
          -- Exference must sanitize them before that strict boundary.
          preferredVarName 1 listType @?= "a"
          expressionNameHints expression @?= Map.singleton 1 "a"
          renderExpression Generated.Unqualified expression
            @?= Right "\\a -> a"
      , testCase "validated canonical search stays lazy and total" $ do
          let goal = input_goalType identityInput
          sourceHints <- expectRight $ mkExferenceSourceTypeVariableHints
            goal $ Map.singleton "a" 0
          target <- checkedIdentifierTarget "lazyIdentity"
          environment <- expectRight $ sealLegacyEnvironment identityInput
          results <- expectRight $ findQueryResultsInEnvironmentEither
            target sourceHints environment (legacyInputQuery identityInput)
          assertBool "canonical search produced no first batch"
            $ not $ null $ take 1 results
          let candidates = concatMap
                (SharedSearch.batchCandidates . SharedQuery.resultSearch)
                results
          assertBool "canonical identity search produced no candidate"
            $ not $ null candidates
          assertBool "canonical candidates lost the checked target"
            $ all ((== target) . Generated.clauseName
                . SharedCandidate.candidateOutput) candidates
          mapM_ (\result ->
              let batch = SharedQuery.resultSearch result
                  expectedEvidence = case SharedSearch.batchCandidates batch of
                    [] -> SharedQuery.NoEvidence
                    _ : _ -> SharedQuery.ValidatedCandidates
              in SharedQuery.resultEvidence result @?= expectedEvidence)
            results
          case candidates of
            candidate : _ -> do
              let details = SharedCandidate.candidateDetails candidate
              Map.lookup (SharedType.RigidVariable 0)
                (exferenceTypeVariableHints details) @?= Just "a"
            [] -> fail "canonical identity search produced no candidate"
          case results of
            [] -> fail "canonical identity search produced no terminal batch"
            firstResult : remaining -> SharedSearch.batchProgress
                (SharedQuery.resultSearch
                  $ lastElement firstResult remaining) @?=
              SharedSearch.Completed SharedSearch.Finished
      , testCase "type hints follow every leading forall layer" $ do
          let function = TypeArrow (TypeVar 4) (TypeVar 9)
              goal = TypeForall [4] []
                $ TypeForall [9] []
                $ TypeArrow function function
              sourceNames = Map.fromList
                [("inner", 9), ("outer", 4)]
              input = identityInput {input_goalType = goal}
          sourceHints <- expectRight
            $ mkExferenceSourceTypeVariableHints goal sourceNames
          target <- checkedIdentifierTarget "nestedHints"
          environment <- expectRight $ sealLegacyEnvironment input
          results <- expectRight $ findQueryResultsInEnvironmentEither
            target sourceHints environment (legacyInputQuery input)
          case concatMap
              (SharedSearch.batchCandidates . SharedQuery.resultSearch)
              results of
            candidate : _ -> exferenceTypeVariableHints
                (SharedCandidate.candidateDetails candidate) @?= Map.fromList
              [ (SharedType.FlexibleVariable 4, "outer")
              , (SharedType.FlexibleVariable 9, "inner")
              , (SharedType.RigidVariable 0, "outer")
              , (SharedType.RigidVariable 1, "inner")
              ]
            [] -> fail "leading-forall hint search produced no identity"
      ]
  , testGroup "Haskell AST conversion"
      [ testCase "shared clauses preserve every generated pattern form" $ do
          definition <- expectRight $ SharedName.mkIdentifier "match"
          checkedDefinition <- expectRight
            $ Generated.mkDefinitionName definition
          justName <- expectRight $ SharedName.mkIdentifier "Just"
          let preferred local = case local of
                0 -> "tuplePart"
                1 -> "whole"
                _ -> "part"
              options = Generated.RenderOptions
                Generated.Unqualified preferred []
              clause = Generated.FunctionClause checkedDefinition
                [ Generated.Wildcard
                , Generated.TuplePattern
                    [Generated.Bind (0 :: Int), Generated.Wildcard]
                , Generated.As 1
                    $ Generated.Constructor justName [Generated.Bind 2]
                ]
                $ Generated.Tuple
                    [Generated.Local 0, Generated.Local 1, Generated.Local 2]
          converted <- expectRight
            $ generatedFunctionClauseToHaskellSrc options clause
          case converted of
            HSE.FunBind _
                [HSE.Match _ (HSE.Ident _ "match")
                  [ HSE.PWildCard _
                  , HSE.PTuple _ HSE.Boxed
                      [ HSE.PVar _ (HSE.Ident _ "tuplePart")
                      , HSE.PWildCard _
                      ]
                  , HSE.PAsPat _ (HSE.Ident _ "whole")
                      (HSE.PApp _
                        (HSE.UnQual _ (HSE.Ident _ "Just"))
                        [HSE.PVar _ (HSE.Ident _ "part")])
                  ]
                  (HSE.UnGuardedRhs _
                    (HSE.Tuple _ HSE.Boxed [_, _, _])) Nothing] -> pure ()
            declaration -> fail $ "unexpected generated clause: "
              ++ show declaration
      , testCase "shared expressions preserve tuples, lets, and cases" $ do
          trueName <- expectRight $ SharedName.mkIdentifier "True"
          falseName <- expectRight $ SharedName.mkIdentifier "False"
          justName <- expectRight $ SharedName.mkIdentifier "Just"
          let preferred local = case local of
                0 -> "left"
                1 -> "right"
                _ -> "value"
              options = Generated.RenderOptions
                Generated.Unqualified preferred []
              expression = Generated.Let
                (Generated.TuplePattern
                  [Generated.Bind (0 :: Int), Generated.Bind 1])
                (Generated.Tuple
                  [Generated.Global trueName, Generated.Global falseName])
                $ Generated.Let
                    (Generated.Constructor justName [Generated.Bind 2])
                    (Generated.Apply
                      (Generated.Global justName) (Generated.Local 0))
                    $ Generated.Case (Generated.Local 2)
                      [ ( Generated.Wildcard
                        , Generated.Tuple
                            [Generated.Local 1, Generated.Local 2]
                        )
                      ]
          converted <- expectRight
            $ generatedExpressionToHaskellSrc options expression
          case converted of
            HSE.Let _
                (HSE.BDecls _
                  [ HSE.PatBind _
                      (HSE.PTuple _ HSE.Boxed [_, _])
                      (HSE.UnGuardedRhs _
                        (HSE.Tuple _ HSE.Boxed [_, _])) Nothing
                  , HSE.PatBind _
                      (HSE.PParen _
                        (HSE.PApp _
                          (HSE.UnQual _ (HSE.Ident _ "Just")) [_]))
                      (HSE.UnGuardedRhs _ (HSE.App _ _ _)) Nothing
                  ])
                (HSE.Case _
                  (HSE.Var _ (HSE.UnQual _ (HSE.Ident _ "value")))
                  [HSE.Alt _ (HSE.PWildCard _)
                    (HSE.UnGuardedRhs _
                      (HSE.Tuple _ HSE.Boxed [_, _])) Nothing]) -> pure ()
            result -> fail $ "unexpected generated expression: " ++ show result
      , testCase "shared conversion reports lexical scope failures" $
          generatedExpressionToHaskellSrc
              (Generated.defaultRenderOptions show)
              (Generated.Local (7 :: Int)) @?=
            Left (HaskellSrcScopeError $ Generated.UnboundLocal 7)
      , testCase "lowercase names use Var rather than Con" $ do
          converted <- expectRight
            $ expressionToHaskellSrc 0 (ExpName $ name "id")
          case converted of
            HSE.Var{} -> pure ()
            expression -> fail $ "expected Var, got " ++ show expression
      , testCase "uppercase names use Con" $ do
          converted <- expectRight
            $ expressionToHaskellSrc 0 (ExpName $ name "Just")
          case converted of
            HSE.Con{} -> pure ()
            expression -> fail $ "expected Con, got " ++ show expression
      , testCase "qualified lowercase names retain Var and qualification" $ do
          converted <- expectRight $ expressionToHaskellSrc 2
            $ ExpName $ validQualifiedName ["Data", "List"] "map"
          case converted of
            HSE.Var _ (HSE.Qual _ (HSE.ModuleName _ "Data.List")
                (HSE.Ident _ "map")) -> pure ()
            expression -> fail $ "unexpected qualified value: " ++ show expression
      , testCase "negative qualification levels are unqualified" $ do
          converted <- expectRight $ expressionToHaskellSrc (-1)
            $ ExpName $ validQualifiedName ["Data", "List"] "map"
          case converted of
            HSE.Var _ (HSE.UnQual _ (HSE.Ident _ "map")) -> pure ()
            expression -> fail $ "unexpected negative-level value: "
              ++ show expression
      , testCase "qualified operators use Symbol names" $ do
          converted <- expectRight $ expressionToHaskellSrc 2
            $ ExpName $ validQualifiedName ["Control", "Applicative"] "<*>"
          case converted of
            HSE.Var _ (HSE.Qual _ (HSE.ModuleName _ "Control.Applicative")
                (HSE.Symbol _ "<*>")) -> pure ()
            expression -> fail $ "unexpected qualified operator: " ++ show expression
      , testCase "Unicode operators remain symbols with either qualification" $ do
          unqualified <- expectRight $ expressionToHaskellSrc 0
            $ ExpName $ validQualifiedName [] "⊕"
          qualified <- expectRight $ expressionToHaskellSrc 2
            $ ExpName $ validQualifiedName ["Math", "Operators"] "⊕"
          case (unqualified, qualified) of
            ( HSE.Var _ (HSE.UnQual _ (HSE.Symbol _ "⊕"))
              , HSE.Var _ (HSE.Qual _ (HSE.ModuleName _ "Math.Operators")
                  (HSE.Symbol _ "⊕"))
              ) -> pure ()
            expressions -> fail $ "unexpected Unicode operators: "
              ++ show expressions
      , testCase "symbolic constructors use Symbol in patterns" $ do
          expression <- expectRight $ expressionToHaskellSrc 0
            $ ExpCaseMatch
                (ExpName $ name "value")
                [(name ":+:", [(1, TypeVar 0), (2, TypeVar 1)],
                  ExpVar 1 (TypeVar 0))]
          case expression of
            HSE.Case _ _ [HSE.Alt _
                (HSE.PApp _ (HSE.UnQual _ (HSE.Symbol _ ":+:")) [_, _]) _ _] ->
              case HSE.parseExp (HSE.prettyPrint expression) of
                HSE.ParseOk _ -> pure ()
                failure -> fail $ "rendered pattern does not parse: "
                  ++ show failure
            _ -> fail $ "unexpected constructor pattern: " ++ show expression
      , testCase "constructor applications use constructor operators" $ do
          expression <- expectRight $ expressionToHaskellSrc 0
            $ ExpApply
                (ExpApply (ExpName $ name ":+:") (ExpName $ name "Left"))
                (ExpName $ name "Right")
          case expression of
            HSE.InfixApp _ _
                (HSE.QConOp _ (HSE.UnQual _ (HSE.Symbol _ ":+:"))) _ ->
              pure ()
            _ -> fail $ "constructor application used a variable operator: "
              ++ show expression
      , testCase "infix conversion preserves either operand tree" $ do
          let leftValue = Generated.Global $ name "LeftValue"
              middleValue = Generated.Global $ name "MiddleValue"
              rightValue = Generated.Global $ name "RightValue"
              infixExpression left right = Generated.Apply
                (Generated.Apply (Generated.Global $ name "<+>") left)
                right
              sources =
                [ ( "left-nested infix"
                  , infixExpression
                      (infixExpression leftValue middleValue) rightValue
                  )
                , ( "left let"
                  , infixExpression
                      (Generated.Let (Generated.Bind (0 :: Int)) leftValue
                        $ Generated.Local 0)
                      rightValue
                  )
                , ( "left case"
                  , infixExpression
                      (Generated.Case leftValue
                        [(Generated.Wildcard, middleValue)])
                      rightValue
                  )
                ]
              options = Generated.RenderOptions
                Generated.Unqualified (const "bound") []
          sources `forM_` \(label, source) -> do
            converted <- expectRight
              $ generatedExpressionToHaskellSrc options source
            case HSE.parseExp $ HSE.prettyPrint converted of
              HSE.ParseOk reparsed ->
                assertEqual (label ++ " changed after pretty-printing")
                  (fmap (const ()) converted) (fmap (const ()) reparsed)
              failure -> fail $ label ++ " no longer parses: " ++ show failure
      , testCase "symbolic type constructors use a legal binder fallback" $ do
          symbolic <- expectRight $ mkQualifiedName [] ":+:"
          converted <- expectRight $ expressionToHaskellSrc 0
            $ ExpLambda 1 (TypeCons symbolic)
            $ ExpVar 1 $ TypeCons symbolic
          case converted of
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
          unqualified <- expectRight $ expressionToHaskellSrc 0 expression
          case unqualified of
            HSE.Lambda _ [HSE.PVar _ (HSE.Ident _ binder)]
                (HSE.Var _ (HSE.UnQual _ (HSE.Ident _ occurrence))) -> do
              binder @?= "a'"
              occurrence @?= "a"
            rendered -> fail $ "capturing unqualified render: " ++ show rendered
          qualified <- expectRight $ expressionToHaskellSrc 2 expression
          case qualified of
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
          converted <- expectRight $ expressionToHaskellSrc 0 expression
          case converted of
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
      , testCase "holes use their collision-free allocated names" $ do
          global <- expectRight $ mkQualifiedName ["M"] "_a"
          converted <- expectRight $ expressionToHaskellSrc 0
            $ ExpApply (ExpName global) (ExpHole 1)
          case converted of
            HSE.App _ _
                (HSE.Var _ (HSE.UnQual _ (HSE.Ident _ hole))) ->
              hole @?= "_a'"
            rendered -> fail $ "unexpected allocated hole: " ++ show rendered
      , testCase "checked expressions reject free locals" $
          case expressionToHaskellSrc 0 $ ExpVar 7 $ TypeVar 0 of
            Left failure -> failure @?=
              ExpressionScopeError (Generated.UnboundLocal 7)
            Right rendered -> fail $ "checked conversion accepted a free local: "
              ++ show rendered
      , testCase "function conversion reserves its declaration name" $ do
          target <- expectRight $ SharedName.mkIdentifier "a"
          let expression = ExpLambda 1 (TypeVar 0)
                $ ExpLambda 2 (TypeVar 1)
                $ ExpVar 1 $ TypeVar 0
          case functionToHaskellSrc 0 target expression of
            Right (HSE.FunBind _
                [HSE.Match _ (HSE.Ident _ function)
                  [ HSE.PVar _ (HSE.Ident _ parameter)
                    , HSE.PVar _ (HSE.Ident _ secondParameter)
                    ]
                  (HSE.UnGuardedRhs _
                    (HSE.Var _ (HSE.UnQual _ (HSE.Ident _ body)))) Nothing]) -> do
                function @?= "a"
                parameter @?= "a'"
                secondParameter @?= "b"
                body @?= parameter
            result -> fail $ "capturing function render: " ++ show result
      , testCase "checked functions reject definition capture" $ do
          target <- expectRight $ SharedName.mkIdentifier "a"
          global <- expectRight $ mkQualifiedName ["M"] "a"
          case functionToHaskellSrc 0 target $ ExpName global of
            Left failure -> failure @?= ExpressionSyntaxError
              (Generated.GlobalDefinitionCapture target
                global Generated.Unqualified)
            Right rendered -> fail $ "checked conversion created recursion: "
              ++ show rendered
      , testCase "compatibility functions validate raw definitions once" $ do
          invalidTarget <- expectRight $ SharedName.mkIdentifier "Result"
          value <- expectRight $ mkQualifiedName [] "value"
          functionToHaskellSrc 0 invalidTarget (ExpName value) @?=
            Left (ExpressionSyntaxError
              $ Generated.InvalidFunctionName invalidTarget)
          -- Keep the compatibility API's historical failure ordering: clause
          -- scope is diagnosed before definition-name syntax.
          functionToHaskellSrc 0 invalidTarget
              (ExpVar 7 $ TypeVar 0) @?=
            Left (ExpressionScopeError $ Generated.UnboundLocal 7)
      , testCase "checked operator definitions use symbolic names" $ do
          target <- expectRight $ SharedName.mkOperator "<+>"
          let expression = ExpLambda 1 (TypeVar 0) $ ExpVar 1 $ TypeVar 0
          case functionToHaskellSrc 0 target expression of
            Right (HSE.FunBind _
                [HSE.Match _ (HSE.Symbol _ "<+>") [_] _ _]) -> pure ()
            result -> fail $ "unexpected operator definition: " ++ show result
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
      , testCase "let inlining does not capture a lambda variable" $ do
          let ty = TypeVar 20
              expression = ExpLet 0 ty (ExpVar 1 ty)
                $ ExpLambda 1 ty
                $ ExpVar 0 ty
          assertBool "lambda capture changed the expression"
            $ simplifyExpression expression == expression
      , testCase "lambda and let shadowing hide outer uses" $ do
          let ty = TypeVar 20
              source = ExpName $ name "source"
              seed = ExpName $ name "seed"
              lambdaShadow = ExpLet 0 ty source
                $ ExpLambda 0 ty
                $ ExpVar 0 ty
              letShadow = ExpLet 0 ty source
                $ ExpLet 0 ty seed
                $ ExpVar 0 ty
              letRightHandScope = ExpLet 0 ty source
                $ ExpLet 0 ty (ExpVar 0 ty)
                $ ExpVar 0 ty
              letCapture = ExpLet 0 ty (ExpVar 1 ty)
                $ ExpLet 1 ty seed
                $ ExpApply (ExpVar 0 ty)
                $ ExpApply (ExpVar 1 ty) (ExpVar 1 ty)
          assertBool "shadowed lambda use retained its outer let"
            $ simplifyExpression lambdaShadow ==
                ExpLambda 0 ty (ExpVar 0 ty)
          assertBool "shadowed let use retained its outer binding"
            $ simplifyExpression letShadow == seed
          assertBool "let binder incorrectly scoped over its right-hand side"
            $ simplifyExpression letRightHandScope == source
          assertBool "inner let captured an inlined free variable"
            $ simplifyExpression letCapture == letCapture
      , testCase "let-pattern binders are scope-safe" $ do
          let ty = TypeVar 20
              constructor = name "Mk"
              scrutinee = ExpName $ name "scrutinee"
              source = ExpName $ name "source"
              patternShadow = ExpLet 0 ty source
                $ ExpLetMatch constructor [(0, ty)] scrutinee
                $ ExpVar 0 ty
              patternRightHandScope = ExpLet 0 ty source
                $ ExpLetMatch constructor [(0, ty)] (ExpVar 0 ty)
                $ ExpVar 0 ty
              patternCapture = ExpLet 0 ty (ExpVar 1 ty)
                $ ExpLetMatch constructor [(1, ty)] scrutinee
                $ ExpVar 0 ty
          assertBool "pattern shadow retained its outer let"
            $ simplifyExpression patternShadow ==
                ExpLetMatch constructor [(0, ty)] scrutinee (ExpVar 0 ty)
          assertBool "pattern binder incorrectly scoped over its scrutinee"
            $ simplifyExpression patternRightHandScope ==
                ExpLetMatch constructor [(0, ty)] source (ExpVar 0 ty)
          assertBool "pattern binder captured an inlined free variable"
            $ simplifyExpression patternCapture == patternCapture
      , testCase "case binders affect only their own alternatives" $ do
          let ty = TypeVar 20
              firstConstructor = name "First"
              secondConstructor = name "Second"
              scrutinee = ExpName $ name "scrutinee"
              source = ExpName $ name "source"
              branchShadow = ExpLet 0 ty source
                $ ExpCaseMatch scrutinee
                    [ (firstConstructor, [(0, ty)], ExpVar 0 ty)
                    , (secondConstructor, [], ExpVar 0 ty)
                    ]
              branchCapture = ExpLet 0 ty (ExpVar 1 ty)
                $ ExpCaseMatch scrutinee
                    [ (firstConstructor, [(1, ty)], ExpVar 0 ty)
                    , (secondConstructor, [], ExpName $ name "other")
                    ]
              expectedShadow = ExpCaseMatch scrutinee
                [ (firstConstructor, [(0, ty)], ExpVar 0 ty)
                , (secondConstructor, [], source)
                ]
          assertBool "case binder leaked into another alternative"
            $ simplifyExpression branchShadow == expectedShadow
          assertBool "case binder captured an inlined free variable"
            $ simplifyExpression branchCapture == branchCapture
      , testCase "case occurrences saturate across unshadowed alternatives" $ do
          let ty = TypeVar 20
              firstConstructor = name "First"
              shadowingConstructor = name "Shadowing"
              finalConstructor = name "Final"
              scrutinee = ExpName $ name "scrutinee"
              source = ExpName $ name "source"
              expression = ExpLet 0 ty source
                $ ExpCaseMatch scrutinee
                    [ (firstConstructor, [], ExpVar 0 ty)
                    , (shadowingConstructor, [(0, ty)], ExpVar 0 ty)
                    , (finalConstructor, [], ExpVar 0 ty)
                    ]
          assertBool "multiple branch uses incorrectly permitted inlining"
            $ simplifyExpression expression == expression
      , testCase "accepts a typed identity" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let variable = TypeVar 0
              identity = ExpLambda 1 variable (ExpVar 1 variable)
              classEnvironment = mkQueryClassEnv staticClasses []
          checkExpression classEnvironment [] []
            (TypeArrow variable variable) [] identity @?= Right ()
      , testCase "reuses one opaque context across checked candidates" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let integer = TypeCons $ name "Int"
              goal = TypeArrow integer integer
              classEnvironment = mkQueryClassEnv staticClasses []
              environment = EnvDictionary [] [] staticClasses
              identity local = ExpLambda local integer
                $ ExpVar local integer
          plan <- expectRight $ planRigidInstantiation
            (mkRigidInstantiationContext environment) [] goal
          context <- expectRight $ prepareExpressionCheckContext plan
            classEnvironment [] [] goal
          checkExpressionInContext context [] (identity 1) @?= Right ()
          checkExpressionInContext context [] (identity 2) @?= Right ()
          mismatchedPlan <- expectRight $ planRigidInstantiation
            (mkRigidInstantiationContext environment) []
            $ TypeForall [9] [] integer
          case prepareExpressionCheckContext mismatchedPlan classEnvironment
              [] [] (TypeForall [8] [] integer) of
            Left failure -> failure @?=
              RigidInstantiationPlanMismatch [9] [8]
            Right _ -> fail "a mismatched checker plan was accepted"
      , testCase "rejects a checker plan allocated for another environment" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let goal = TypeForall [0] []
                $ TypeArrow (TypeVar 0) (TypeVar 0)
              synthesizedName = name "f"
              occupiedType = TypeConstant 0
              functions =
                [ FunctionBinding
                    (TypeArrow occupiedType occupiedType)
                    synthesizedName 0 [] []
                ]
              classEnvironment = mkQueryClassEnv staticClasses []
              emptyEnvironment = EnvDictionary [] [] staticClasses
              occupiedEnvironment = EnvDictionary functions [] staticClasses
          foreignPlan <- expectRight $ planRigidInstantiation
            (mkRigidInstantiationContext emptyEnvironment) [] goal
          rigidInstantiations foreignPlan @?= [(0, 0)]
          case prepareExpressionCheckContext foreignPlan classEnvironment
              functions [] goal of
            Left failure -> failure @?=
              RigidInstantiationTargetCollision [0]
            Right _ -> fail "a foreign checker plan was accepted"
          checkExpressionWithRigidInstantiation foreignPlan classEnvironment
              functions [] goal [] (ExpName synthesizedName)
            @?= Left (RigidInstantiationTargetCollision [0])

          localPlan <- expectRight $ planRigidInstantiation
            (mkRigidInstantiationContext occupiedEnvironment) [] goal
          rigidInstantiations localPlan @?= [(0, 1)]
          case checkExpressionWithRigidInstantiation localPlan classEnvironment
              functions [] goal [] (ExpName synthesizedName) of
            Left TypeMismatch{} -> pure ()
            result -> fail $ "a colliding binding passed the correct plan: "
              ++ show result
          let conservativeIdentity = ExpLambda 7 (TypeConstant 1)
                $ ExpVar 7 (TypeConstant 1)
          checkExpressionWithRigidInstantiation localPlan classEnvironment
              [] [] goal [] conservativeIdentity @?= Right ()
      , testCase "preflights cyclic checker class and pattern arities" $ do
          let className = name "Unary"
              unary = HsTypeClass className [0] []
              integer = TypeCons $ name "Int"
              malformedConstraint = HsConstraint className $ repeat integer
          staticClasses <- expectRight $ mkStaticClassEnv [unary] []
          let classEnvironment = mkQueryClassEnv staticClasses []
              constrainedGoal = TypeForall [] [malformedConstraint] integer
          checkExpression classEnvironment [] [] constrainedGoal []
              (ExpName $ name "unused")
            @?= Left (InvalidCheckClassConstraint $ ConstraintArityMismatch
              QueryConstraint className 1 2)

          let boxType = TypeCons $ name "Box"
              constructor = name "MkBox"
              deconstructor = DeconstructorBinding boxType
                [ConstructorBinding constructor [integer]] False
              boxValueName = name "boxValue"
              integerValueName = name "integerValue"
              functions =
                [ FunctionBinding boxType boxValueName 0 [] []
                , FunctionBinding integer integerValueName 0 [] []
                ]
              cyclicVariables = repeat (1, integer)
              expected = Left $ PatternArity constructor 1 2
              letExpression = ExpLetMatch constructor cyclicVariables
                (ExpName boxValueName) (ExpName integerValueName)
              caseExpression = ExpCaseMatch (ExpName boxValueName)
                [(constructor, cyclicVariables, ExpName integerValueName)]
          checkExpression classEnvironment functions [deconstructor]
            integer [] letExpression @?= expected
          checkExpression classEnvironment functions [deconstructor]
            integer [] caseExpression @?= expected
      , testCase "opens constrained goals with search's own assumptions" $ do
          -- Regression: the public checker discarded each prenex layer's
          -- constraints while opening the goal, so it falsely rejected
          -- results that live search accepts for a constrained query.
          let showConstraint = HsConstraint (name "Show") [TypeVar 0]
              stringType = TypeCons $ name "String"
              showBinding = FunctionBinding stringType (name "show") 0
                [showConstraint] [TypeVar 0]
              goal = TypeForall [0] [showConstraint]
                $ TypeArrow (TypeVar 0) stringType
          staticClasses <- expectRight
            $ mkStaticClassEnv [HsTypeClass (name "Show") [0] []] []
          let input = identityInput
                { input_goalType = goal
                , input_envFuncs = [showBinding]
                , input_envClasses = staticClasses
                }
          (expression, residual, _) <- maybe
            (fail "constrained search found no solution") pure
            $ findOneExpression input
          residual @?= []
          checkExpression (mkQueryClassEnv staticClasses []) [showBinding] []
            goal [] expression @?= Right ()
      , testCase "canonicalizes expected residual constraints" $ do
          -- Regression: caller-supplied residual constraints were compared
          -- without canonicalization, so a semantically equal
          -- application-form tuple spelling failed the comparison against
          -- the canonicalized inferred side.
          pairName <- expectRight $ mkBoxedTupleName 2
          staticClasses <- expectRight
            $ mkStaticClassEnv [HsTypeClass (name "Show") [0] []] []
          let firstType = TypeConstant 0
              secondType = TypeConstant 1
              structuralPair = TypeTuple Boxed [firstType, secondType]
              applicationPair = TypeApp
                (TypeApp (TypeCons pairName) firstType) secondType
              stringType = TypeCons $ name "String"
              showBinding = FunctionBinding stringType (name "show") 0
                [HsConstraint (name "Show") [TypeVar 0]] [TypeVar 0]
              pairBinding =
                FunctionBinding structuralPair (name "pair") 0 [] []
              expression = ExpApply (ExpName $ name "show")
                (ExpName $ name "pair")
              expected = [HsConstraint (name "Show") [applicationPair]]
          checkExpression (mkQueryClassEnv staticClasses [])
            [showBinding, pairBinding] [] stringType expected expression
            @?= Right ()
      , testCase "canonicalizes ground inferred constraints before solving" $ do
          let className = name "C"
              integer = TypeCons $ name "Int"
              boolean = TypeCons $ name "Bool"
              structural = HsConstraint className
                [TypeArrow integer boolean]
              application = HsConstraint className
                [ TypeApp
                    (TypeApp (TypeCons SharedName.functionName) integer)
                    boolean
                ]
              bindingName = name "constrained"
              binding = FunctionBinding integer bindingName 0
                [application] []
              expression = ExpName bindingName
          staticClasses <- expectRight
            $ mkStaticClassEnv [HsTypeClass className [0] []] []
          checkExpression (mkQueryClassEnv staticClasses []) [binding] []
            integer [structural] expression @?= Right ()
          checkExpression (mkQueryClassEnv staticClasses [structural])
            [binding] [] integer [] expression @?= Right ()
      , testCase "rejects malformed known query assumptions" $ do
          let className = name "C"
              malformed = HsConstraint className []
              integer = TypeCons $ name "Int"
              bindingName = name "constrained"
              binding = FunctionBinding integer bindingName 0
                [malformed] []
          staticClasses <- expectRight
            $ mkStaticClassEnv [HsTypeClass className [0] []] []
          checkExpression (mkQueryClassEnv staticClasses [malformed])
            [binding] [] integer [] (ExpName bindingName)
            @?= Left (InvalidCheckClassConstraint
              $ ConstraintArityMismatch QueryConstraint className 1 0)
      , testCase "rejects malformed known binding constraints" $ do
          let className = name "C"
              malformed = HsConstraint className []
              integer = TypeCons $ name "Int"
              malformedName = name "malformed"
              malformedBinding = FunctionBinding integer malformedName 0
                [malformed] []
              seedName = name "seed"
              seed = FunctionBinding integer seedName 0 [] []
          staticClasses <- expectRight
            $ mkStaticClassEnv [HsTypeClass className [0] []] []
          checkExpression (mkQueryClassEnv staticClasses [])
            [malformedBinding, seed] [] integer [] (ExpName seedName)
            @?= Left (InvalidCheckClassConstraint
              $ ConstraintArityMismatch
                  (BindingConstraint malformedName) className 1 0)
      , testCase "rejects malformed known goal constraints" $ do
          let className = name "C"
              malformed = HsConstraint className []
              integer = TypeCons $ name "Int"
              goal = TypeForall [] [malformed] integer
              seedName = name "seed"
              seed = FunctionBinding integer seedName 0 [] []
          staticClasses <- expectRight
            $ mkStaticClassEnv [HsTypeClass className [0] []] []
          checkExpression (mkQueryClassEnv staticClasses [])
            [seed] [] goal [] (ExpName seedName)
            @?= Left (InvalidCheckClassConstraint
              $ ConstraintArityMismatch QueryConstraint className 1 0)
      , testCase "rejects malformed known expected constraints" $ do
          let className = name "C"
              malformed = HsConstraint className []
              integer = TypeCons $ name "Int"
              bindingName = name "constrained"
              binding = FunctionBinding integer bindingName 0
                [malformed] []
          staticClasses <- expectRight
            $ mkStaticClassEnv [HsTypeClass className [0] []] []
          checkExpression (mkQueryClassEnv staticClasses [])
            [binding] [] integer [malformed] (ExpName bindingName)
            @?= Left (InvalidCheckClassConstraint
              $ ConstraintArityMismatch QueryConstraint className 1 0)
      , testCase "retains unknown classes as nominal constraints" $ do
          let external = HsConstraint (name "External") []
              integer = TypeCons $ name "Int"
              bindingName = name "externalValue"
              binding = FunctionBinding integer bindingName 0 [external] []
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          checkExpression (mkQueryClassEnv staticClasses [external])
            [binding] [] integer [] (ExpName bindingName)
            @?= Right ()
      , testCase "rejects duplicate function identities before lookup" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let duplicateName = name "ambiguous"
              integer = TypeCons $ name "Int"
              boolean = TypeCons $ name "Bool"
              firstBinding = FunctionBinding integer duplicateName 0 [] []
              secondBinding = FunctionBinding boolean duplicateName 0 [] []
          checkExpression (mkQueryClassEnv staticClasses [])
            [firstBinding, secondBinding] [] integer []
            (ExpName duplicateName) @?= Left
              (InvalidCheckEnvironmentBindings
                $ DuplicateFunctionIdentities [duplicateName])
      , testCase "rejects duplicate constructor identities before lookup" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let duplicateName = name "Ambiguous"
              firstType = TypeCons $ name "First"
              secondType = TypeCons $ name "Second"
              firstDeconstructor = DeconstructorBinding firstType
                [ConstructorBinding duplicateName []] False
              secondDeconstructor = DeconstructorBinding secondType
                [ConstructorBinding duplicateName []] False
              expression = ExpLambda 1 firstType
                $ ExpCaseMatch (ExpVar 1 firstType)
                    [(duplicateName, [], ExpVar 1 firstType)]
          checkExpression (mkQueryClassEnv staticClasses []) []
            [firstDeconstructor, secondDeconstructor]
            (TypeArrow firstType firstType) [] expression @?= Left
              (InvalidCheckEnvironmentBindings
                $ DuplicateConstructorIdentities [duplicateName])
      , testCase "rejects duplicate datatype eliminator identities" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let duplicateType = TypeCons $ name "Duplicate"
              firstDeconstructor = DeconstructorBinding duplicateType
                [ConstructorBinding (name "First") []] False
              secondDeconstructor = DeconstructorBinding duplicateType
                [ConstructorBinding (name "Second") []] False
              expression = ExpLambda 1 duplicateType
                $ ExpVar 1 duplicateType
          checkExpression (mkQueryClassEnv staticClasses []) []
            [firstDeconstructor, secondDeconstructor]
            (TypeArrow duplicateType duplicateType) [] expression @?= Left
              (InvalidCheckEnvironmentBindings
                $ DuplicateDeconstructorIdentities [name "Duplicate"])
      , testCase "rejects invalid unused environment names" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let integer = TypeCons $ name "Int"
              seedName = name "seed"
              seed = FunctionBinding integer seedName 0 [] []
              arrowName = validQualifiedName [] "->"
              invalidFunction = FunctionBinding
                integer arrowName 0 [] []
              invalidConstructorName = name "notAConstructor"
              invalidDeconstructor = DeconstructorBinding
                (TypeCons $ name "Container")
                [ConstructorBinding invalidConstructorName []] False
              checked functions deconstructors = checkExpression
                (mkQueryClassEnv staticClasses []) functions deconstructors
                integer [] $ ExpName seedName
          checked [invalidFunction, seed] [] @?= Left
            (InvalidCheckEnvironmentSyntax
              $ InvalidFunctionBindingSyntax arrowName
              $ Generated.InvalidGlobalExpression SharedName.functionName)
          checked [seed] [invalidDeconstructor] @?= Left
            (InvalidCheckEnvironmentSyntax
              $ InvalidConstructorBindingSyntax invalidConstructorName
              $ Generated.InvalidConstructorPattern invalidConstructorName)
      , testCase "shares signed-finite environment ratings with search" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let integer = TypeCons $ name "Int"
              seedName = name "seed"
              seed = FunctionBinding integer seedName 0 [] []
              invalidName = name "unusedInvalidRating"
              invalidPenalty = Penalty $ 0 / 0
              invalid = FunctionBinding
                integer invalidName invalidPenalty [] []
              checked binding = checkExpression
                (mkQueryClassEnv staticClasses []) [seed, binding] []
                integer [] $ ExpName seedName
          case checked invalid of
            Left (InvalidCheckEnvironmentRatings
                (NonFiniteFunctionRating actualName (Penalty value))) -> do
              actualName @?= invalidName
              assertBool "checker rating failure lost NaN" $ isNaN value
            result -> fail $ "checker accepted an unused non-finite rating: "
              ++ show result
          case mkExferenceEnvironment
              $ EnvDictionary [seed, invalid] [] staticClasses of
            Left (InvalidHeuristic field (Penalty value)) -> do
              field @?= show invalidName
              assertBool "search rating failure lost NaN" $ isNaN value
            Left failure -> fail $ "unexpected search rating failure: "
              ++ show failure
            Right _ -> fail "search accepted a non-finite rating"
          checked invalid {functionPenalty = Penalty (-3.5)} @?= Right ()
      , testCase "rejects duplicate generated pattern binders" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let integer = TypeCons $ name "Int"
              pairType = TypeCons $ name "Pair"
              pairConstructor = name "Pair"
              deconstructor = DeconstructorBinding pairType
                [ConstructorBinding pairConstructor [integer, integer]] False
              duplicateBinder = 2
              expression = ExpLambda 1 pairType
                $ ExpCaseMatch (ExpVar 1 pairType)
                    [ ( pairConstructor
                      , [ (duplicateBinder, integer)
                        , (duplicateBinder, integer)
                        ]
                      , ExpVar duplicateBinder integer
                      )
                    ]
          checkExpression (mkQueryClassEnv staticClasses []) []
            [deconstructor] (TypeArrow pairType integer) [] expression @?=
              Left (InvalidCheckExpressionScope
                $ Generated.DuplicatePatternBinder duplicateBinder)
      , testCase "accepts unused rank-N environment capabilities" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let integer = TypeCons $ name "Int"
              polymorphic = TypeForall [0] [] $ TypeVar 0
              seedName = name "seed"
              seed = FunctionBinding integer seedName 0 [] []
              nestedName = name "nested"
              nestedFunction = FunctionBinding
                polymorphic nestedName 0 [] []
              boxType = TypeApp (TypeCons $ name "Box") $ TypeVar 0
              nestedDeconstructor = DeconstructorBinding boxType
                [ConstructorBinding (name "Nested") [polymorphic]] False
              checked functions deconstructors = checkExpression
                (mkQueryClassEnv staticClasses []) functions deconstructors
                integer [] $ ExpName seedName
          checked [nestedFunction, seed] [] @?= Right ()
          checked [seed] [nestedDeconstructor] @?= Right ()
      , testCase "accepts rank-N atoms in the complete class environment" $ do
          let className = name "C"
              polymorphic = TypeForall [1] [] $ TypeVar 1
              headConstraint = HsConstraint className [polymorphic]
              instanceDeclaration = HsInstance [] headConstraint
              integer = TypeCons $ name "Int"
              seedName = name "seed"
              seed = FunctionBinding integer seedName 0 [] []
          staticClasses <- expectRight $ mkStaticClassEnv
            [HsTypeClass className [0] []] [instanceDeclaration]
          map snd (environmentConstraints
              $ EnvDictionary [] [] staticClasses) @?= [headConstraint]
          checkExpression (mkQueryClassEnv staticClasses [])
            [seed] [] integer [] (ExpName seedName) @?= Right ()
      , testCase "checks alpha-equivalent rank-N generated annotations" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let polymorphic = TypeForall [0] [] $ TypeVar 0
              renamed = TypeForall [7] [] $ TypeVar 7
              expression = ExpLambda 1 polymorphic $ ExpVar 1 polymorphic
          checkExpression (mkQueryClassEnv staticClasses []) [] []
            (TypeArrow renamed polymorphic) [] expression @?= Right ()
      , testCase "instantiates each polymorphic local occurrence independently" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let integer = TypeCons $ name "Int"
              boolean = TypeCons $ name "Bool"
              result = TypeCons $ name "Result"
              polymorphic = TypeForall [0] []
                $ TypeArrow (TypeVar 0) (TypeVar 0)
              combineName = name "combine"
              integerName = name "integer"
              booleanName = name "boolean"
              functions =
                [ FunctionBinding result combineName 0 [] [integer, boolean]
                , FunctionBinding integer integerName 0 [] []
                , FunctionBinding boolean booleanName 0 [] []
                ]
              use local ty value = ExpApply
                (ExpVar local $ TypeArrow ty ty)
                (ExpName value)
              expression = ExpLambda 1 polymorphic
                $ ExpApply
                    (ExpApply (ExpName combineName)
                      $ use 1 integer integerName)
                    (use 1 boolean booleanName)
          checkExpression (mkQueryClassEnv staticClasses []) functions []
            (TypeArrow polymorphic result) [] expression @?= Right ()
      , testCase "a flexible occurrence annotation requests instantiation" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let unit = TypeTuple Boxed []
              polymorphic = TypeForall [0] [] $ TypeVar 0
              expression = ExpLambda 1 polymorphic $ ExpVar 1 (TypeVar 2)
          checkExpression (mkQueryClassEnv staticClasses []) [] []
            (TypeArrow polymorphic unit) [] expression @?= Right ()
          let vacuousUnit = TypeForall [] [] unit
              wrappedOccurrence = ExpLambda 1 polymorphic
                $ ExpVar 1 $ TypeForall [] [] $ TypeVar 2
          checkExpression (mkQueryClassEnv staticClasses []) [] []
            (TypeArrow polymorphic vacuousUnit) [] wrappedOccurrence @?= Right ()
      , testCase "a quantified occurrence annotation requests checked subsumption" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let polymorphic = TypeForall [0] [] $ TypeVar 0
              distinct = TypeForall [1] []
                $ TypeArrow (TypeVar 1) (TypeVar 1)
              expression = ExpLambda 1 polymorphic $ ExpVar 1 distinct
          checkExpression (mkQueryClassEnv staticClasses []) [] []
            (TypeArrow polymorphic distinct) [] expression @?= Right ()

          let integer = TypeCons $ name "Int"
              lessGeneral = TypeForall [2] []
                $ TypeArrow integer integer
              universalIdentity = TypeForall [3] []
                $ TypeArrow (TypeVar 3) (TypeVar 3)
              lessGeneralUse = ExpLambda 2 lessGeneral
                $ ExpVar 2 universalIdentity
              providerIdentity = TypeForall [4] []
                $ TypeArrow (TypeVar 4) (TypeVar 4)
              impredicative = TypeForall [5] []
                $ TypeArrow
                    (TypeForall [6] []
                      $ TypeArrow (TypeVar 6) (TypeVar 6))
                    (TypeForall [7] []
                      $ TypeArrow (TypeVar 7) (TypeVar 7))
              impredicativeUse = ExpLambda 3 providerIdentity
                $ ExpVar 3 impredicative
              -- The two quantified atoms differ (the second mentions the
              -- requested binder), so no single binder image covers both.
              correlatedImpredicative = TypeForall [5] []
                $ TypeArrow
                    (TypeForall [6] []
                      $ TypeArrow (TypeVar 6) (TypeVar 6))
                    (TypeForall [7] []
                      $ TypeArrow (TypeVar 7) (TypeVar 5))
              correlatedImpredicativeUse = ExpLambda 3 providerIdentity
                $ ExpVar 3 correlatedImpredicative
              rejects candidateGoal candidate = case checkExpression
                  (mkQueryClassEnv staticClasses []) [] [] candidateGoal []
                  candidate of
                Left TypeMismatch{} -> pure ()
                actual -> fail $ "checker accepted invalid quantified subsumption: "
                  ++ show actual
          rejects (TypeArrow lessGeneral universalIdentity) lessGeneralUse
          -- Guarded impredicativity: the provider binder is solved with the
          -- quantified atom the requested scheme itself supplies, so this
          -- occurrence now checks; inventing a quantifier still fails.
          checkExpression (mkQueryClassEnv staticClasses []) [] []
            (TypeArrow providerIdentity impredicative) [] impredicativeUse
            @?= Right ()
          rejects (TypeArrow providerIdentity correlatedImpredicative)
            correlatedImpredicativeUse
      , testCase "rejects an invalid monomorphic provider annotation" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let integer = TypeCons $ name "Int"
              boolean = TypeCons $ name "Bool"
              polymorphic = TypeForall [0] []
                $ TypeArrow (TypeVar 0) (TypeVar 0)
              invalidUse = TypeArrow integer boolean
              expression = ExpLambda 1 polymorphic $ ExpVar 1 invalidUse
          case checkExpression (mkQueryClassEnv staticClasses []) [] []
              (TypeArrow polymorphic invalidUse) [] expression of
            Left TypeMismatch{} -> pure ()
            actual -> fail $ "checker accepted an inconsistent forall use: "
              ++ show actual
      , testCase "empty cases require a matching empty deconstructor" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let emptyType = TypeCons $ name "Empty"
              otherType = TypeCons $ name "Other"
              integer = TypeCons $ name "Int"
              goal = TypeArrow emptyType integer
              expression = ExpLambda 1 emptyType
                $ ExpCaseMatch (ExpVar 1 emptyType) []
              mismatchedEmpty = DeconstructorBinding otherType [] False
              matchingEmpty = DeconstructorBinding emptyType [] False
              inhabited = DeconstructorBinding emptyType
                [ConstructorBinding (name "NotEmpty") []] False
              expected = Left
                $ EmptyCaseWithoutMatchingDeconstructor emptyType
          mapM_ (\deconstructors ->
              checkExpression (mkQueryClassEnv staticClasses []) []
                deconstructors goal [] expression @?= expected)
            [[], [mismatchedEmpty], [inhabited]]
          checkExpression (mkQueryClassEnv staticClasses []) []
            [mismatchedEmpty, matchingEmpty] goal [] expression @?= Right ()
      , testCase "rejects headless deconstructors before checking patterns" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let integer = TypeCons $ name "Int"
              boolean = TypeCons $ name "Bool"
              constructor = name "Bogus"
              resultName = name "truth"
              result = FunctionBinding boolean resultName 0 [] []
              headless = DeconstructorBinding (TypeVar 0)
                [ConstructorBinding constructor []] False
              expression = ExpLambda 1 integer
                $ ExpCaseMatch (ExpVar 1 integer)
                    [(constructor, [], ExpName resultName)]
          checkExpression (mkQueryClassEnv staticClasses []) [result]
            [headless] (TypeArrow integer boolean) [] expression
            @?= Left (InvalidCheckDeconstructor
              $ MissingDeconstructorNominalHead $ TypeVar 0)
      , testCase "rejects function-headed deconstructors" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let integer = TypeCons $ name "Int"
              boolean = TypeCons $ name "Bool"
              functionType = TypeApp
                (TypeApp (TypeCons SharedName.functionName) integer) boolean
              constructor = name "Function"
              resultName = name "zero"
              result = FunctionBinding integer resultName 0 [] []
              functionHead = DeconstructorBinding functionType
                [ConstructorBinding constructor []] False
              expression = ExpLambda 1 functionType
                $ ExpCaseMatch (ExpVar 1 functionType)
                    [(constructor, [], ExpName resultName)]
          checkExpression (mkQueryClassEnv staticClasses []) [result]
            [functionHead] (TypeArrow functionType integer) [] expression
            @?= Left (InvalidCheckDeconstructor
              $ FunctionDeconstructorHead SharedName.functionName)
      , testCase "rejects escaping deconstructor field variables" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let boxName = name "Box"
              boxType = TypeApp (TypeCons boxName) $ TypeVar 0
              constructor = name "MkBox"
              escaping = DeconstructorBinding boxType
                [ConstructorBinding constructor [TypeVar 1]] False
              integer = TypeCons $ name "Int"
              seedName = name "seed"
              seed = FunctionBinding integer seedName 0 [] []
          checkExpression (mkQueryClassEnv staticClasses []) [seed]
            [escaping] integer [] (ExpName seedName)
            @?= Left (InvalidCheckDeconstructor
              $ UnboundDeconstructorFields boxName constructor [1])
      , testCase "rank-N field binders do not escape their scope" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let boxName = name "PolyBox"
              boxType = TypeCons boxName
              constructor = name "MkPolyBox"
              polymorphic = TypeForall [1] []
                $ TypeArrow (TypeVar 1) (TypeVar 1)
              rankN = DeconstructorBinding boxType
                [ConstructorBinding constructor [polymorphic]] False
              integer = TypeCons $ name "Int"
              seedName = name "seed"
              seed = FunctionBinding integer seedName 0 [] []
          validateDeconstructorBinding rankN @?= Right ()
          checkExpression (mkQueryClassEnv staticClasses []) [seed]
            [rankN] integer [] (ExpName seedName) @?= Right ()
      , testCase "deconstructor type errors precede nominal shape errors" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let integer = TypeCons $ name "Int"
              headless = DeconstructorBinding (TypeVar 0) [] False
              malformed = TypeTuple Boxed [integer]
              malformedDeconstructor = DeconstructorBinding malformed [] False
              seedName = name "seed"
              seed = FunctionBinding integer seedName 0 [] []
          checkExpression (mkQueryClassEnv staticClasses []) [seed]
            [headless, malformedDeconstructor]
            integer [] (ExpName seedName) @?= Left
              (InvalidCheckType malformed
                $ InvalidSynthesisType
                $ SharedType.InvalidTupleTypeArity Boxed 1)
      , testCase "checks structural tuples against constructor applications" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          pairName <- expectRight $ mkBoxedTupleName 2
          let firstType = TypeVar 0
              secondType = TypeConstant 1
              structural = TypeTuple Boxed [firstType, secondType]
              application = TypeApp
                (TypeApp (TypeCons pairName) firstType) secondType
              identity = ExpLambda 1 application
                $ ExpVar 1 structural
          checkExpression (mkQueryClassEnv staticClasses []) [] []
            (TypeArrow structural structural) [] identity @?= Right ()
      , testCase "instantiates higher-kinded heads to tuple constructors" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let firstType = TypeConstant 0
              secondType = TypeConstant 1
              structural = TypeTuple Boxed [firstType, secondType]
              polymorphicHead = TypeApp
                (TypeApp (TypeVar 2) firstType) secondType
              identity = ExpLambda 1 polymorphicHead
                $ ExpVar 1 structural
          checkExpression (mkQueryClassEnv staticClasses []) [] []
            (TypeArrow structural structural) [] identity @?= Right ()
      , testCase "instantiates higher-kinded heads to the function constructor" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let parameterType = TypeConstant 0
              resultType = TypeConstant 1
              structural = TypeArrow parameterType resultType
              polymorphicHead = TypeApp
                (TypeApp (TypeVar 2) parameterType) resultType
              identity = ExpLambda 1 polymorphicHead
                $ ExpVar 1 structural
          checkExpression (mkQueryClassEnv staticClasses []) [] []
            (TypeArrow structural structural) [] identity @?= Right ()
      , testCase "rejects malformed native types before checking" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let malformed = TypeTuple Boxed [TypeCons $ name "Int"]
              goal = TypeArrow malformed malformed
              identity = ExpLambda 1 malformed $ ExpVar 1 malformed
          checkExpression (mkQueryClassEnv staticClasses []) [] []
            goal [] identity @?= Left
              (InvalidCheckType goal
                $ InvalidSynthesisType
                $ SharedType.InvalidTupleTypeArity Boxed 1)
      , testCase "validates raw inputs before rigid planning" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let integer = TypeCons $ name "Int"
              invalidType = TypeForallNative
                [SharedType.RigidVariable 4] [] integer
              invalidBinding = FunctionBinding
                invalidType (name "invalid") 0 [] []
              seedName = name "seed"
              seed = FunctionBinding integer seedName 0 [] []
          checkExpression (mkQueryClassEnv staticClasses [])
            [invalidBinding, seed] [] integer [] (ExpName seedName) @?=
              Left (InvalidCheckType invalidType $ RigidForallBinder 4)
      , testCase "fresh variables do not wrap onto boundary annotations" $ do
          staticClasses <- expectRight $ mkStaticClassEnv [] []
          let firstType = TypeConstant 0
              secondType = TypeConstant 1
              goal = TypeArrow firstType
                $ TypeArrow (TypeArrow firstType secondType) secondType
              application = ExpApply
                (ExpVar 2 $ TypeVar maxBound)
                (ExpVar 1 $ TypeVar minBound)
              expression = ExpLambda 1 (TypeVar minBound)
                $ ExpLambda 2 (TypeVar maxBound) application
          checkExpression (mkQueryClassEnv staticClasses []) [] []
            goal [] expression @?= Right ()
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

neutralName :: String -> SharedName.Name
neutralName = name

neutralVariable :: Int -> SynthesisVariable
neutralVariable = SharedType.FlexibleVariable

neutralParameter
  :: SynthesisVariable
  -> SharedDeclaration.TypeParameter SynthesisVariable Void
neutralParameter variable = SharedDeclaration.TypeParameter variable Nothing

neutralValue
  :: SharedName.Name
  -> SharedType.Type SynthesisVariable
  -> SharedDeclaration.Declaration SynthesisVariable Void ()
neutralValue valueName valueType = SharedDeclaration.ValueDeclaration
  $ SharedDeclaration.ValueSignature () valueName valueType

lowerNeutralDeclarations
  :: [SharedDeclaration.Declaration SynthesisVariable Void ()]
  -> IO EnvDictionary
lowerNeutralDeclarations declarations = do
  inventory <- expectRight $ SharedInventory.mkInventory
    SharedKindInference.OpenKindInventory declarations
  prepared <- expectRight
    $ prepareSynthesisInventory inventory
  pure $ preparedSynthesisBackend prepared

name :: String -> QualifiedName
name = validQualifiedName []

-- Static test fixtures fail loudly if malformed while exercising the same
-- checked constructors that production callers use.
validQualifiedName :: [String] -> String -> QualifiedName
validQualifiedName modules spelling =
  either (error . ("invalid static qualified name: " ++) . show) id
    $ mkQualifiedName modules spelling

validTupleName :: Int -> QualifiedName
validTupleName arity =
  either (error . ("invalid static tuple name: " ++) . show) id
    $ mkBoxedTupleName arity

-- A deliberately tiny but complete frontend inventory.  Keeping the
-- constructor functions beside their structural declarations makes it easy
-- for the source-boundary tests above to perturb exactly one side of the
-- required bijection.
maybeLikeSourceEnvironment :: SourceEnvironment
maybeLikeSourceEnvironment = SourceEnvironment
  { sourceBindings =
      [ SourceFunction
          $ FunctionBinding maybeType nothingName (Penalty 1.25) [] []
      , SourceFunction
          $ FunctionBinding maybeType justName (Penalty 2.75) [] [TypeVar 0]
      , SourceFunction $ FunctionBinding maybeType (name "defaultMaybe")
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
    let leftResult = canonicalType $ snd
          $ applySubsts leftSubstitutions left
        rightResult = canonicalType $ snd
          $ applySubsts rightSubstitutions right
    assertBool
      ( "unclosed symmetric substitution for " ++ show left ++ " ~ " ++ show right
        ++ ": " ++ show leftResult ++ " /= " ++ show rightResult
        ++ " from " ++ show leftSubstitutions
        ++ " and " ++ show rightSubstitutions
      )
      (leftResult == rightResult)

assertSharedUnifierCloses :: HsType -> HsType -> IO ()
assertSharedUnifierCloses left right = case unifyShared left right of
  Nothing -> pure ()
  Just substitutions -> do
    let leftResult = canonicalType $ snd $ applySubsts substitutions left
        rightResult = canonicalType $ snd $ applySubsts substitutions right
    assertBool
      ( "unclosed shared substitution for " ++ show left ++ " ~ " ++ show right
        ++ ": " ++ show leftResult ++ " /= " ++ show rightResult
        ++ " from " ++ show substitutions
      )
      (leftResult == rightResult)

assertOffsetUnifierCloses :: Int -> HsType -> HsType -> IO ()
assertOffsetUnifierCloses offset left right =
  case unifyOffset left (HsTypeOffset right offset) of
    Nothing -> pure ()
    Just (leftSubstitutions, rightSubstitutions) -> do
      let shiftedRight = incVarIds (+ offset) right
          leftResult = canonicalType $ snd
            $ applySubsts leftSubstitutions left
          rightResult = canonicalType $ snd
            $ applySubsts rightSubstitutions shiftedRight
      assertBool
        ( "unclosed offset substitution for " ++ show left ++ " ~ " ++ show right
          ++ ": " ++ show leftResult ++ " /= " ++ show rightResult
          ++ " from " ++ show leftSubstitutions
          ++ " and " ++ show rightSubstitutions
        )
        (leftResult == rightResult)

assertRightUnifierCloses :: HsType -> HsType -> IO ()
assertRightUnifierCloses left right = case unifyRight left right of
  Nothing -> pure ()
  Just substitutions -> do
    let leftResult = canonicalType left
        rightResult = canonicalType $ snd $ applySubsts substitutions right
    assertBool
      ( "unclosed right substitution for " ++ show left ++ " ~ " ++ show right
        ++ ": " ++ show leftResult ++ " /= " ++ show rightResult
        ++ " from " ++ show substitutions
      )
      (leftResult == rightResult)

canonicalType :: HsType -> HsType
canonicalType = SharedType.canonicalizeType

parseTypePure :: String -> Either Diagnostic (HsType, TypeVarIndex)
parseTypePure = parseTypeWithModePure $ haskellSrcExtsParseMode "test"

-- A source round trip is stable when reparsing the rendered type produces the
-- same text again.  This deliberately compares source identities rather than
-- raw numeric IDs: renaming a nested binder can change the parser's allocation
-- order without changing which textual variables are free.
assertTypeRenderRoundTrip :: String -> IO ()
assertTypeRenderRoundTrip source = do
  (parsed, sourceNames) <- expectRight $ parseTypePure source
  let rendered = showHsType sourceNames parsed
  (reparsed, reparsedNames) <- expectRight $ parseTypePure rendered
  assertEqual ("unstable type rendering for " ++ source)
    rendered $ showHsType reparsedNames reparsed

parseTypeWithTestResolver
  :: TypeResolver
  -> String
  -> Either String (HsType, TypeVarIndex)
parseTypeWithTestResolver resolver source = case
    HSE.parseTypeWithMode (haskellSrcExtsParseMode "type-operator-test") source of
  HSE.ParseFailed location message -> Left $ show location ++ ": " ++ message
  HSE.ParseOk syntaxType -> runIdentity $ runExceptT
    $ convertTypeNoDeclWithResolver resolver Nothing syntaxType

isLoaderSummary :: String -> Bool
isLoaderSummary message = any (`isPrefixOf` message)
  ["got ", "and ", "(-> "]

parseTypeWithModePure
  :: HSE.ParseMode
  -> String
  -> Either Diagnostic (HsType, TypeVarIndex)
parseTypeWithModePure mode source = runIdentity
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
  LoadReport environmentResult diagnostics <-
    environmentFromPath environmentDirectory
  checkedEnvironment <- expectRight environmentResult
  let environment = checkedSourceProjection checkedEnvironment
  pure
    ( sourceFunctions environment
    , sourceClasses environment
    , map diagnosticMessage diagnostics
    )

runLoad
  :: IO (LoadReport result)
  -> IO (Either EnvironmentLoadError result, [String])
runLoad action = do
  LoadReport result diagnostics <- action
  pure (result, map diagnosticMessage diagnostics)

unsupportedFromSource :: String -> IO [UnsupportedVocabularyOccurrence]
unsupportedFromSource = unsupportedFromSourceWith []

unsupportedFromSourceWith
  :: [HSE.KnownExtension]
  -> String
  -> IO [UnsupportedVocabularyOccurrence]
unsupportedFromSourceWith extraExtensions source =
  withTemporaryFile source $ \modulePath -> do
    LoadReport result _ <- parseModules
      [ ( enableExtensions extraExtensions
            $ haskellSrcExtsParseMode modulePath
        , modulePath
        )
      ]
    occurrences <- expectUnsupportedVocabulary result
    mapM_ (assertStructured modulePath) occurrences
    pure occurrences
 where
  assertStructured modulePath occurrence = do
    let value = unsupportedVocabularyDiagnostic occurrence
    diagnosticSeverity value @?= Error
    diagnosticCode value @?= Just "EXF_UNSUPPORTED_VOCABULARY"
    diagnosticSource value @?= Just modulePath
    assertBool ("invalid diagnostic location fallback: " ++ show value)
      $ case diagnosticSpan value of
          Just span' ->
            let start = sourceStart span'
                end = sourceEnd span'
            in sourceLine start > 0
              && sourceColumn start > 0
              && end >= start
          Nothing -> case diagnosticContext value of
            [context] ->
              "haskell-src-exts supplied an invalid source location: "
                `isPrefixOf` context
            _ -> False

expectUnsupportedVocabulary
  :: Either EnvironmentLoadError result
  -> IO [UnsupportedVocabularyOccurrence]
expectUnsupportedVocabulary result = case result of
  Left (UnsupportedSourceVocabulary (firstOccurrence :| remaining)) ->
    pure $ firstOccurrence : remaining
  Left failure -> fail $ "unexpected load failure: " ++ show failure
  Right _ -> fail "unsupported source vocabulary was accepted"

occurrenceStartLine :: UnsupportedVocabularyOccurrence -> Maybe Int
occurrenceStartLine occurrence =
  sourceLine . sourceStart
    <$> diagnosticSpan (unsupportedVocabularyDiagnostic occurrence)

enableExtensions
  :: [HSE.KnownExtension]
  -> HSE.ParseMode
  -> HSE.ParseMode
enableExtensions requested mode = mode
  { HSE.extensions = map HSE.EnableExtension requested
      ++ HSE.extensions mode
  }

-- Compare the historical typed-chunk view with the canonical shared result
-- produced from the same checked engine batch.  The historical counters are
-- machine-sized, but these focused fixtures remain small enough for their
-- projection back to exact Natural metadata to be lossless.
assertSameBatch
  :: String
  -> Generated.DefinitionName
  -> ExferenceChunkElement
  -> ExferenceResult
  -> IO ()
assertSameBatch label target historical canonical = do
  assertEqual (label ++ " progress") (historicalProgress status)
    $ SharedSearch.batchProgress batch
  assertEqual (label ++ " metadata") expectedMetadata
    $ SharedSearch.batchMetadata batch
  assertEqual (label ++ " candidates") expectedCandidates actualCandidates
  assertEqual (label ++ " evidence") expectedEvidence
    $ SharedQuery.resultEvidence canonical
 where
  status = chunkStatus historical
  batch = SharedQuery.resultSearch canonical
  expectedMetadata = ExferenceBatchMetadata
    { exferenceBindingUsages = Map.map fromIntegral
        $ chunkBindingUsages historical
    , exferenceQueuePruned = fromIntegral $ searchQueuePruned status
    , exferenceDepthPruned = fromIntegral $ searchDepthPruned status
    }
  expectedCandidates =
    [ ( Generated.functionClauseFromExpression target
          $ toGeneratedExpression expression
      , constraints
      , statistics
      , expressionNameHints expression
      )
    | (expression, constraints, statistics) <- chunkElements historical
    ]
  actualCandidates =
    [ ( SharedCandidate.candidateOutput candidate
      , SharedCandidate.candidateResidualConstraints candidate
      , exferenceCandidateStats details
      , exferenceLocalNameHints details
      )
    | candidate <- SharedSearch.batchCandidates batch
    , let details = SharedCandidate.candidateDetails candidate
    ]
  expectedEvidence = case expectedCandidates of
    [] -> SharedQuery.NoEvidence
    _ : _ -> SharedQuery.ValidatedCandidates

-- Independent oracle relating the retained historical completion record to
-- the canonical shared progress emitted from the same private engine batch.
-- Caller-built contradictory statuses have no conversion API after the
-- transitional batch layer is retired; this helper sees engine output only.
historicalProgress :: SearchStatus -> SharedSearch.Progress
historicalProgress status
  | queuePruned < 0 || depthPruned < 0 =
      error "engine produced negative historical pruning metadata"
  | otherwise = case searchCompletion status of
      SearchRunning -> SharedSearch.Continuing
      SearchExhausted
        | null pruningReasons -> SharedSearch.Completed SharedSearch.Finished
        | otherwise -> error
            "engine marked a pruned historical search as exhausted"
      SearchStepLimitReached -> SharedSearch.Completed
        $ SharedSearch.Truncated
        $ SharedSearch.StepLimitReached :| pruningReasons
      SearchPruned -> case pruningReasons of
        reason : remaining -> SharedSearch.Completed
          $ SharedSearch.Truncated $ reason :| remaining
        [] -> error "engine reported historical pruning without a reason"
      SearchIdentifierSpaceExhausted -> SharedSearch.Completed
        $ SharedSearch.Truncated
        $ SharedSearch.IdentifierSpaceExhausted :| pruningReasons
 where
  queuePruned = searchQueuePruned status
  depthPruned = searchDepthPruned status
  pruningReasons =
    [ SharedSearch.QueueLimitPruned $ fromIntegral queuePruned
    | queuePruned > 0
    ] ++
    [ SharedSearch.DepthLimitPruned $ fromIntegral depthPruned
    | depthPruned > 0
    ]

checkedIdentifierTarget :: String -> IO Generated.DefinitionName
checkedIdentifierTarget spelling = do
  targetName <- expectRight $ SharedName.mkIdentifier spelling
  expectRight $ Generated.mkDefinitionName targetName

legacyInputEnvironment :: ExferenceInput -> EnvDictionary
legacyInputEnvironment input = EnvDictionary
  { environmentFunctions = input_envFuncs input
  , environmentDeconstructors = input_envDeconsS input
  , environmentClasses = input_envClasses input
  }

legacyInputQuery :: ExferenceInput -> ExferenceQuery
legacyInputQuery input = ExferenceQuery
  { queryGoalType = input_goalType input
  , queryExcludedBindings = Set.empty
  , querySearchOptions = legacyInputOptions input
  }

legacyInputOptions :: ExferenceInput -> ExferenceOptions
legacyInputOptions input = ExferenceOptions
  { exferenceAllowUnused = input_allowUnused input
  , exferenceAllowResidualConstraints = input_allowConstraints input
  , exferenceConstraintDeferralSteps = input_allowConstraintsStopStep input
  , exferenceMultiConstructorPatterns = input_multiPM input
  , exferenceMaximumSteps = input_maxSteps input
  , exferenceMaximumQueueSize = input_maxQueueSize input
  , exferenceMaximumDepth = input_maxDepth input
  , exferenceHeuristics = input_heuristicsConfig input
  }

mapQueryOptions
  :: (ExferenceOptions -> ExferenceOptions)
  -> ExferenceQuery
  -> ExferenceQuery
mapQueryOptions update query = query
  { querySearchOptions = update $ querySearchOptions query
  }

sealLegacyEnvironment
  :: ExferenceInput
  -> Either ExferenceInputError ExferenceEnvironment
sealLegacyEnvironment = mkExferenceEnvironment . legacyInputEnvironment

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

lastElement :: value -> [value] -> value
lastElement latest [] = latest
lastElement _ (next : remaining) = lastElement next remaining

expectRight :: Show problem => Either problem result -> IO result
expectRight = either (fail . show) pure

expectSourceEnvironment
  :: [(FilePath, String)]
  -> IO SourceEnvironment
expectSourceEnvironment sources = do
  LoadReport result _ <- parseModuleSources sources
  expectRight result

sourceFunctionNamed
  :: SourceEnvironment
  -> QualifiedName
  -> IO FunctionBinding
sourceFunctionNamed environment wanted = case find
    ((== wanted) . functionName) $ sourceFunctions environment of
  Just binding -> pure binding
  Nothing -> fail $ "source function was not loaded: " ++ show wanted

expectBindingScopeFailure
  :: [(FilePath, String)]
  -> IO [String]
expectBindingScopeFailure sources = do
  LoadReport result _ <- parseModuleSources sources
  case result of
    Left (BindingDeclarationErrors failures) -> pure
      $ map extractionErrorMessage $ NonEmpty.toList failures
    Left failure -> fail $ "unexpected source-scope failure: " ++ show failure
    Right environment -> fail $ "out-of-scope source was accepted: "
      ++ show environment

validSourceSpan :: Int -> Int -> Int -> Int -> SourceSpan
validSourceSpan startLine startColumn endLine endColumn =
  either (error . show) id $ do
    start <- mkSourcePosition startLine startColumn
    end <- mkSourcePosition endLine endColumn
    mkSourceSpan start end

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

withTemporaryDirectoryFiles
  :: [(FilePath, String)]
  -> (FilePath -> IO a)
  -> IO a
withTemporaryDirectoryFiles files action = do
  temporaryDirectory <- getTemporaryDirectory
  bracket
    (do
      (path, handle) <- openTempFile temporaryDirectory
        "exference-loader-environment"
      hClose handle
      removeFile path
      createDirectory path
      forM_ files $ \(fileName, contents) ->
        writeFile (path ++ "/" ++ fileName) contents
      pure path)
    removePathForcibly
    action

classEnvironmentFromSources
  :: [String]
  -> IO (Either ClassEnvironmentLoadError StaticClassEnv)
classEnvironmentFromSources sources = do
  modules <- mapM expectParsedModule sources
  let result = runIdentity $ loadClassEnvironment [] Map.empty modules
  pure $ loadedStaticClassEnvironment <$> result
