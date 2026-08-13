module Main (main) where

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Set as Set
import Data.Void (Void)
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertEqual, testCase)

import Language.Haskell.Exference.Core.Candidate
  ( emptyExferenceSourceTypeVariableHints )
import Language.Haskell.Exference.Core.ConstraintSolver (filterUnresolved)
import Language.Haskell.Exference.Core.Expression
  ( Expression (..), toGeneratedExpression )
import Language.Haskell.Exference.Core.ExferenceStats (ExferenceStats (..))
import Language.Haskell.Exference.Core.Internal.ExpressionCheck
  ( CheckedExpressionEvidence
  , ExpressionCheckError (..)
  , ExferenceTermGraphAbsence (..)
  , ExferenceTermGraphAvailability (..)
  , ExferenceTermGraphConstructionLimit (..)
  , checkedExpressionTermGraph
  , checkedExpressionTypeApplicationOriginReferences
  , checkedExpressionTypeApplicationOrigins
  , checkedTypeApplicationOriginId
  , checkedTypeApplicationOriginOwner
  , checkedTypeApplicationOriginSource
  , checkedTypeApplicationOriginStepObligations
  , checkedTypeApplicationOriginStepResult
  , checkedTypeApplicationOriginStepSelected
  , checkedTypeApplicationOriginStepSlot
  , checkedTypeApplicationOriginStepSource
  , checkedTypeApplicationOriginSteps
  , checkExpression
  , checkExpressionInContextWithNestedRigidProvenanceEvidence
  , prepareExpressionCheckContext
  , prepareExpressionCheckContextWithSchemes
  )
import Language.Haskell.Exference.Core.FunctionBinding
  ( ConstructorBinding (..)
  , DeconstructorBinding (..)
  , EnvDictionary (..)
  , FunctionBinding (..)
  )
import Language.Haskell.Exference.Core.Declaration
  ( prepareSynthesisInventory
  , preparedSynthesisBackend
  , preparedSynthesisSchemes
  )
import qualified Language.Haskell.Exference.Core.Internal.Exference as E
import Language.Haskell.Exference.Core.Internal.FlexibleIds
  ( allocateNamespace )
import Language.Haskell.Exference.Core.Internal.Options
  ( ExferenceHeuristicsConfig (..)
  , ExferenceOptions (..)
  , defaultHeuristicsConfig
  )
import Language.Haskell.Exference.Core.Internal.Polytype
  ( GroundProviderInstantiation (..)
  , candidateProviderInstantiations
  , assignmentProviderInstantiations
  , groundProviderInstantiations
  , instantiateLeadingForallsWith
  , quantifiedProviderSubsumes
  )
import Language.Haskell.Exference.Core.Internal.RigidScope
  ( RigidEscape (..)
  , emptyRigidScope
  , nestedRigidProvenance
  , registerRigidScope
  , validateRigidSubstitutions
  )
import Language.Haskell.Exference.Core.Internal.ScopedConstraint
import Language.Haskell.Exference.Core.Internal.Testing
import Language.Haskell.Exference.Core.Internal.VariableSupply
  ( supplyFromIdentifiers )
import Language.Haskell.Exference.Core.Score (Penalty (..))
import qualified Language.Haskell.Exference.Core.Score as Score
import Language.Haskell.Exference.Core.RigidInstantiation
  ( mkRigidInstantiationContext, planRigidInstantiation )
import Language.Haskell.Exference.Core.Types
import qualified Language.Haskell.Synthesis.Candidate as SharedCandidate
import qualified Language.Haskell.Synthesis.Declaration as SharedDeclaration
import qualified Language.Haskell.Synthesis.Generated as Generated
import qualified Language.Haskell.Synthesis.Inventory as SharedInventory
import qualified Language.Haskell.Synthesis.KindInference as SharedKindInference
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Query as SharedQuery
import qualified Language.Haskell.Synthesis.Search as SharedSearch
import qualified Language.Haskell.Synthesis.TypeAtom as SharedTypeAtom
import qualified Language.Haskell.Synthesis.TypedGenerated as Typed

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Exference private engine boundaries"
  [ testCase "rigid scopes reject direct and propagated skolem escapes" $ do
      let opened = registerRigidScope
            (IntSet.singleton 0) [7] emptyRigidScope
      validateRigidSubstitutions opened
        (IntMap.singleton 0 $ TypeConstant 7) @?=
          Left (RigidEscape 0 7)
      propagated <- expectRight $ validateRigidSubstitutions opened
        (IntMap.singleton 0 $ TypeVar 1)
      validateRigidSubstitutions propagated
        (IntMap.singleton 1 $ TypeConstant 7) @?=
          Left (RigidEscape 1 7)
      validateRigidSubstitutions opened
        (IntMap.fromList
          [(0, TypeVar 1), (1, TypeConstant 7)]) @?=
            Left (RigidEscape 1 7)
      case validateRigidSubstitutions opened
          (IntMap.singleton 2 $ TypeConstant 7) of
        Right _ -> pure ()
        Left failure -> fail
          $ "a younger flexible variable was rejected: " ++ show failure
  , testCase "scoped givens resolve only their own obligations" $ do
      let className = name "C"
          integer = TypeCons $ name "Int"
          evidence = HsConstraint className [integer]
          local = ScopedConstraint [evidence] evidence
          sibling = ScopedConstraint [] evidence
      staticClasses <- expectRight
        $ mkStaticClassEnv [HsTypeClass className [0] []] []
      resolveScopedConstraints filterUnresolved
        (mkQueryClassEnv staticClasses []) [local, sibling]
        @?= Just [sibling]
  , testCase "scoped substitutions update givens and obligations together" $ do
      let className = name "C"
          integer = TypeCons $ name "Int"
          evidence ty = HsConstraint className [ty]
          original = ScopedConstraint
            [evidence $ TypeVar 0]
            (evidence $ TypeVar 1)
          (_, substituted) = scopedConstraintApplySubsts
            (IntMap.fromList [(0, integer), (1, integer)]) original
      staticClasses <- expectRight
        $ mkStaticClassEnv [HsTypeClass className [0] []] []
      substituted @?= ScopedConstraint [evidence integer] (evidence integer)
      resolveScopedConstraints filterUnresolved
        (mkQueryClassEnv staticClasses []) [substituted]
        @?= Just []
  , testCase "scoped instance prerequisites retain their lexical givens" $ do
      let baseName = name "Base"
          derivedName = name "Derived"
          integer = TypeCons $ name "Int"
          base ty = HsConstraint baseName [ty]
          derived ty = HsConstraint derivedName [ty]
          instanceRule = HsInstance
            [base $ TypeVar 0]
            (derived $ TypeVar 0)
          local = ScopedConstraint [base integer] (derived integer)
          sibling = ScopedConstraint [] (derived integer)
      staticClasses <- expectRight $ mkStaticClassEnv
        [ HsTypeClass baseName [0] []
        , HsTypeClass derivedName [0] []
        ] [instanceRule]
      resolveScopedConstraints filterUnresolved
        (mkQueryClassEnv staticClasses []) [local, sibling]
        @?= Just [ScopedConstraint [] $ base integer]
  , testCase "prepared queries consume one validated options witness" $ do
      environment <- expectRight $ sealLegacyEnvironment identityInput
      target <- checkedIdentifierTarget "singleOptionValidation"
      let query = legacyInputQuery identityInput
          sourceHints = emptyExferenceSourceTypeVariableHints
            $ E.queryGoalType query
      singleOptionValidationStrictnessForTesting
        target sourceHints environment query @?= Right ()
  , testCase "term identifier exhaustion truncates instead of colliding" $ do
      chunk <- lastCapacityChunk
        (IdentifierCapacities 0 100 100 100) identityInput
      E.chunkStatus chunk @?=
        E.SearchStatus E.SearchIdentifierSpaceExhausted 0 0
      assertBool "an exhausted term-ID branch produced a candidate"
        $ null $ E.chunkElements chunk
  , testCase "identifier truncation survives a successful sibling" $ do
      let integer = TypeCons $ name "Int"
          polymorphic = FunctionBinding
            (TypeVar 0) (name "polymorphic") 0 [] []
          constant = FunctionBinding integer (name "constant") 0 [] []
          input = identityInput
            { E.input_goalType = integer
            , E.input_envFuncs = [polymorphic, constant]
            }
          capacities = IdentifierCapacities 100 0 100 100
      chunk <- lastCapacityChunk capacities input
      E.searchCompletion (E.chunkStatus chunk) @?=
        E.SearchIdentifierSpaceExhausted
      assertBool "a viable sibling was suppressed by identifier exhaustion"
        $ not $ null $ E.chunkElements chunk
      target <- checkedIdentifierTarget "generated"
      environment <- expectRight $ sealLegacyEnvironment input
      results <- expectRight
        $ findQueryResultsWithIdentifierCapacitiesEither
            capacities target
            (emptyExferenceSourceTypeVariableHints $ E.input_goalType input)
            environment (legacyInputQuery input)
      result <- lastResult results
      let batch = SharedQuery.resultSearch result
      SharedQuery.resultEvidence result @?=
        SharedQuery.ValidatedCandidates
      SharedSearch.batchProgress batch @?= SharedSearch.Completed
        (SharedSearch.Truncated
          $ SharedSearch.IdentifierSpaceExhausted :| [])
      assertBool "the direct result lost its checked target"
        $ all ((== target) . Generated.clauseName
            . SharedCandidate.candidateOutput)
        $ SharedSearch.batchCandidates batch
  , testCase "provider forall exhaustion truncates the affected branch" $ do
      let integer = TypeCons $ name "Int"
          polymorphic = TypeForall [0] []
            $ TypeArrow (TypeVar 0) (TypeVar 0)
          input = identityInput
            { E.input_goalType =
                TypeArrow polymorphic $ TypeArrow integer integer
            , E.input_maxSteps = 100
            }
      -- Binder 0 already occupies the only flexible slot. Per-use
      -- instantiation needs one additional spelling and must fail without
      -- wrapping or reusing the binder identity.
      chunk <- lastCapacityChunk
        (IdentifierCapacities 100 1 100 100) input
      E.chunkStatus chunk @?=
        E.SearchStatus E.SearchIdentifierSpaceExhausted 0 0
      assertBool "an exhausted forall instantiation produced a candidate"
        $ null $ E.chunkElements chunk
  , testCase "nested forall introduction is checked and rigid-bounded" $ do
      let result = TypeCons $ name "Result"
          identityScheme = TypeForall [0] []
            $ TypeArrow (TypeVar 0) (TypeVar 0)
          goal = TypeArrow
            (TypeArrow identityScheme result)
            result
          input = identityInput
            { E.input_goalType = goal
            , E.input_maxSteps = 200
            }
      chunks <- expectRight
        $ findExpressionsWithIdentifierCapacitiesEither
            (IdentifierCapacities 100 100 100 100) input
      let expressions =
            [ expression
            | chunk <- chunks
            , (expression, _, _) <- E.chunkElements chunk
            ]
      assertBool "the quantified callback produced no checked candidate"
        $ not $ null expressions
      mapM_ (\expression -> checkExpression
          (mkQueryClassEnv emptyStaticClassEnv []) [] [] goal [] expression
            @?= Right ())
        expressions

      exhausted <- lastCapacityChunk
        (IdentifierCapacities 100 100 0 100) input
      E.searchCompletion (E.chunkStatus exhausted) @?=
        E.SearchIdentifierSpaceExhausted
      assertBool "rigid exhaustion admitted a quantified callback"
        $ null $ E.chunkElements exhausted
  , testCase "nested forall skolems cannot escape through older variables" $ do
      let boolean = TypeCons $ name "Bool"
          callback = TypeForall [1] []
            $ TypeArrow (TypeVar 1) (TypeVar 0)
          use = FunctionBinding boolean (name "use") 0 [] [callback]
          input = identityInput
            { E.input_goalType = boolean
            , E.input_envFuncs = [use]
            , E.input_maxSteps = 200
            }
      chunks <- expectRight
        $ findExpressionsWithIdentifierCapacitiesEither
            (IdentifierCapacities 100 100 100 100) input
      assertBool "an older result meta captured a nested skolem"
        $ null $ concatMap E.chunkElements chunks
  , testCase "bare provider foralls cross the checked result boundary" $ do
      let unit = TypeTuple Boxed []
          vacuousUnit = TypeForall [] [] unit
          polymorphic = TypeForall [0] [] $ TypeVar 0
          input = identityInput
            { E.input_goalType = TypeArrow polymorphic unit
            , E.input_maxSteps = 100
            }
      chunks <- expectRight
        $ findExpressionsWithIdentifierCapacitiesEither
            (IdentifierCapacities 100 100 100 100) input
      assertBool
        "forall a. a was mistaken for opaque forwarding at a flexible use"
        $ not $ null $ concatMap E.chunkElements chunks
      wrappedChunks <- expectRight
        $ findExpressionsWithIdentifierCapacitiesEither
            (IdentifierCapacities 100 100 100 100)
            (input {E.input_goalType = TypeArrow polymorphic vacuousUnit})
      assertBool "a vacuous forall wrapper suppressed provider instantiation"
        $ not $ null $ concatMap E.chunkElements wrappedChunks
      let wrappedOccurrence = ExpLambda 1 polymorphic
            $ ExpVar 1 $ TypeForall [] [] $ TypeVar 2
      checkExpression (mkQueryClassEnv emptyStaticClassEnv []) [] []
        (TypeArrow polymorphic vacuousUnit) [] wrappedOccurrence @?= Right ()
      let distinct = TypeForall [1] []
            $ TypeArrow (TypeVar 1) (TypeVar 1)
      quantifiedChunks <- expectRight
        $ findExpressionsWithIdentifierCapacitiesEither
            (IdentifierCapacities 100 100 100 100)
            (input {E.input_goalType = TypeArrow polymorphic distinct})
      let quantifiedExpressions =
            [ expression
            | chunk <- quantifiedChunks
            , (expression, _, _) <- E.chunkElements chunk
            ]
          requestedOccurrence expression = case expression of
            ExpLambda _ declared (ExpVar _ annotation) ->
              declared == polymorphic && annotation == distinct
            _ -> False
      assertBool "a more-general provider did not subsume a quantified goal"
        $ any requestedOccurrence quantifiedExpressions
      mapM_ (\expression -> checkExpression
          (mkQueryClassEnv emptyStaticClassEnv []) [] []
          (TypeArrow polymorphic distinct) [] expression @?= Right ())
        quantifiedExpressions
      -- Guarded impredicativity end to end: the requested scheme itself
      -- supplies the quantified atom the provider binder is solved with,
      -- and the independent checker accepts every emitted candidate.
      let impredicativeRequested = TypeForall [3] []
            $ TypeArrow
                (TypeForall [4] [] $ TypeArrow (TypeVar 4) (TypeVar 4))
                (TypeForall [5] [] $ TypeArrow (TypeVar 5) (TypeVar 5))
          polymorphicIdentity = TypeForall [1] []
            $ TypeArrow (TypeVar 1) (TypeVar 1)
          impredicativeGoal =
            TypeArrow polymorphicIdentity impredicativeRequested
      impredicativeChunks <- expectRight
        $ findExpressionsWithIdentifierCapacitiesEither
            (IdentifierCapacities 100 100 100 100)
            (input {E.input_goalType = impredicativeGoal})
      let impredicativeExpressions =
            [ expression
            | chunk <- impredicativeChunks
            , (expression, _, _) <- E.chunkElements chunk
            ]
          impredicativeOccurrence expression = case expression of
            ExpLambda _ declared (ExpVar _ annotation) ->
              declared == polymorphicIdentity
                && annotation == impredicativeRequested
            _ -> False
      assertBool "guarded impredicative subsumption found no forwarding"
        $ any impredicativeOccurrence impredicativeExpressions
      mapM_ (\expression -> checkExpression
          (mkQueryClassEnv emptyStaticClassEnv []) [] []
          impredicativeGoal [] expression @?= Right ())
        impredicativeExpressions
  , testCase "quantified provider subsumption stays shallow with guarded impredicativity" $ do
      let integer = TypeCons $ name "Int"
          provider = TypeForall [0, 1] []
            $ TypeArrow (TypeVar 0)
            $ TypeArrow (TypeVar 1) (TypeVar 0)
          specialization = TypeForall [2] []
            $ TypeArrow (TypeVar 2)
            $ TypeArrow (TypeVar 2) (TypeVar 2)
          wrongResult = TypeForall [2, 3] []
            $ TypeArrow (TypeVar 2)
            $ TypeArrow (TypeVar 3) (TypeVar 3)
          lessGeneral = TypeForall [0] []
            $ TypeArrow integer integer
          ordinaryIdentity = TypeForall [2] []
            $ TypeArrow (TypeVar 2) (TypeVar 2)
          impredicative = TypeForall [2] []
            $ TypeArrow
                (TypeForall [3] [] $ TypeArrow (TypeVar 3) (TypeVar 3))
                (TypeForall [4] [] $ TypeArrow (TypeVar 4) (TypeVar 4))
          -- The two quantified atoms differ (the second mentions the
          -- requested binder), so one provider binder cannot cover both.
          correlatedImpredicative = TypeForall [2] []
            $ TypeArrow
                (TypeForall [3] [] $ TypeArrow (TypeVar 3) (TypeVar 3))
                (TypeForall [4] [] $ TypeArrow (TypeVar 4) (TypeVar 2))
          providerIdentity = TypeForall [0] []
            $ TypeArrow (TypeVar 0) (TypeVar 0)
          freeProvider = TypeForall [0] []
            $ TypeArrow (TypeVar 9) (TypeVar 0)
          freeRequested = TypeForall [1] []
            $ TypeArrow (TypeVar 9) (TypeVar 1)
          contextual variable = TypeForall [variable]
            [HsConstraint (name "C") [TypeVar variable]]
            $ TypeVar variable
          rigidProvider = TypeForall [9] [] $ TypeConstant 0
          collidingRequested = TypeForall [0] [] $ TypeVar 0
      quantifiedProviderSubsumes provider specialization @?= True
      quantifiedProviderSubsumes provider wrongResult @?= False
      quantifiedProviderSubsumes lessGeneral ordinaryIdentity @?= False
      -- Guarded impredicativity: the binder is solved with a quantified
      -- atom, but only one the requested scheme itself supplies.
      quantifiedProviderSubsumes providerIdentity impredicative @?= True
      quantifiedProviderSubsumes providerIdentity correlatedImpredicative
        @?= False
      quantifiedProviderSubsumes freeProvider freeRequested @?= False
      quantifiedProviderSubsumes (contextual 0) (contextual 1) @?= False
      quantifiedProviderSubsumes rigidProvider collidingRequested @?= False
  , testCase "provider forall opening preserves nested lexical scopes" $ do
      let outerClass = name "Outer"
          innerClass = name "Inner"
          evidence className variable = HsConstraint className [variable]
          shadowed = TypeForall [0] [evidence outerClass $ TypeVar 0]
            $ TypeForall [0] [evidence innerClass $ TypeVar 0]
            $ TypeVar 0
      case instantiateLeadingForallsWith
          allocateNamespace (supplyFromIdentifiers []) shadowed of
        Just
            ( TypeVar bodyIdentifier
            , [ HsConstraint outerName [TypeVar outerIdentifier]
              , HsConstraint innerName [TypeVar innerIdentifier]
              ]
            , _
            ) -> do
          outerName @?= outerClass
          innerName @?= innerClass
          assertBool "shadowed forall binders reused one fresh identity"
            $ outerIdentifier /= innerIdentifier
          bodyIdentifier @?= innerIdentifier
        actual -> fail $ "unexpected shadowed-forall instantiation: "
          ++ show actual
  , testCase "ground provider evidence separates free and bound identities" $ do
      let outerClass = name "Outer"
          innerClass = name "Inner"
          integer = TypeCons $ name "Int"
          boolean = TypeCons $ name "Bool"
          token = TypeCons $ name "Token"
          evidence className ty = HsConstraint className [ty]
          provider =
            TypeForall [] [evidence outerClass $ TypeVar 0]
              $ TypeForall [0] [evidence innerClass $ TypeVar 0]
                token
          classes =
            [ HsTypeClass outerClass [0] []
            , HsTypeClass innerClass [0] []
            ]
      outerOnly <- expectRight $ mkStaticClassEnv classes
        [HsInstance [] $ evidence outerClass integer]
      complete <- expectRight $ mkStaticClassEnv classes
        [ HsInstance [] $ evidence outerClass integer
        , HsInstance [] $ evidence innerClass boolean
        ]
      groundProviderInstantiations
          (mkQueryClassEnv outerOnly []) provider @?= []
      groundProviderInstantiations
          (mkQueryClassEnv complete []) provider @?=
        [ GroundProviderInstantiation
            { groundProviderArguments = [boolean]
            , groundProviderType = token
            , groundProviderConstraints =
                [ evidence outerClass $ TypeVar 0
                , evidence innerClass boolean
                ]
            }
        ]
  , testCase "query candidates admit only closed context-free foralls" $ do
      let token = TypeCons $ name "Token"
          evidence ty = HsConstraint (name "C") [ty]
          identity binder = TypeForall [binder] []
            $ TypeArrow (TypeVar binder) (TypeVar binder)
          selected = identity 1
          renamed = identity 7
          open = TypeForall [2] []
            $ TypeArrow (TypeVar 2) (TypeVar 3)
          rigidlyOpen = TypeForall [4] []
            $ TypeArrow (TypeVar 4) (TypeConstant 9)
          contextual = TypeForall [5] [evidence $ TypeVar 5]
            $ TypeArrow (TypeVar 5) (TypeVar 5)
          nestedContextual = TypeForall [6] []
            $ TypeArrow
                (TypeForall [8] [evidence $ TypeVar 8]
                  $ TypeVar 8)
                (TypeVar 6)
          provider = TypeForall [0] [] token
      candidateProviderInstantiations
          [selected, renamed, open, rigidlyOpen, contextual, nestedContextual]
          provider @?=
        [ GroundProviderInstantiation
            { groundProviderArguments = [selected]
            , groundProviderType = token
            , groundProviderConstraints = []
            }
        ]
  , testCase "query candidates retain ambient rigids, not flexibles" $ do
      let selected = TypeForall [1] []
            $ TypeArrow (TypeVar 1) (TypeVar 1)
          provider result = TypeForall [0] [] result
          ambientRigid = TypeConstant 9
          ambientFlexible = TypeVar 9
      candidateProviderInstantiations [selected]
          (provider ambientRigid) @?=
        [ GroundProviderInstantiation
            { groundProviderArguments = [selected]
            , groundProviderType = ambientRigid
            , groundProviderConstraints = []
            }
        ]
      candidateProviderInstantiations [selected]
          (provider ambientFlexible) @?= []
  , testCase "exact assignments retain structure, order, and non-vacuous bodies" $ do
      let selected = TypeForall [1] []
            $ TypeArrow (TypeVar 1) (TypeVar 1)
          contextual = TypeForall [15]
            [HsConstraint (name "Eq") [TypeVar 15]]
            $ TypeArrow (TypeVar 15) (TypeVar 15)
          wrapper argument = TypeApp (TypeCons $ name "Wrapper") argument
          provider = TypeForall [0] [] $ wrapper $ TypeVar 0
          contextualProvider = TypeForall [0]
            [HsConstraint (name "C") [TypeVar 0]]
            $ wrapper $ TypeVar 0
          token = TypeCons $ name "Token"
          structural = wrapper selected
          quantified binder body = TypeForall [binder] [] body
          arguments =
            [ quantified 11 $ TypeArrow (TypeVar 11) (TypeVar 11)
            , quantified 12 $ TypeArrow (TypeVar 12) token
            , quantified 13 $ TypeArrow token (TypeVar 13)
            , quantified 14 $ TypeArrow
                (TypeVar 14) (TypeArrow (TypeVar 14) (TypeVar 14))
            ]
          fourBinderProvider = TypeForall [2, 3, 4, 5] [] token
      candidateProviderInstantiations [selected] provider @?= []
      candidateProviderInstantiations [contextual] provider @?= []
      assignmentProviderInstantiations [[selected]] provider @?=
        [ GroundProviderInstantiation
            { groundProviderArguments = [selected]
            , groundProviderType = wrapper selected
            , groundProviderConstraints = []
            }
        ]
      assignmentProviderInstantiations [[contextual]] provider @?=
        [ GroundProviderInstantiation
            { groundProviderArguments = [contextual]
            , groundProviderType = wrapper contextual
            , groundProviderConstraints = []
            }
        ]
      assignmentProviderInstantiations [[selected]] contextualProvider @?= []
      assignmentProviderInstantiations [[structural]]
          (TypeForall [6] [] token) @?=
        [ GroundProviderInstantiation
            { groundProviderArguments = [structural]
            , groundProviderType = token
            , groundProviderConstraints = []
            }
        ]
      assignmentProviderInstantiations [arguments] fourBinderProvider @?=
        [ GroundProviderInstantiation
            { groundProviderArguments = arguments
            , groundProviderType = token
            , groundProviderConstraints = []
            }
        ]
  , testCase "provider instantiation admits six binders and rejects seven" $ do
      let token = TypeCons $ name "WideToken"
          quantified binder body = TypeForall [binder] [] body
          selected = quantified 10
            $ TypeArrow (TypeVar 10) (TypeVar 10)
          exactArguments =
            [ quantified 11 $ TypeArrow (TypeVar 11) (TypeVar 11)
            , quantified 12 $ TypeArrow (TypeVar 12) token
            , quantified 13 $ TypeArrow token (TypeVar 13)
            , quantified 14 $ TypeArrow
                (TypeVar 14) (TypeArrow (TypeVar 14) (TypeVar 14))
            , quantified 15 $ TypeArrow
                (TypeArrow (TypeVar 15) token) (TypeVar 15)
            , quantified 16 $ TypeArrow
                (TypeVar 16) (TypeArrow token (TypeVar 16))
            ]
          seventhArgument = quantified 17 $ TypeArrow
            (TypeArrow (TypeVar 17) token)
            (TypeArrow token (TypeVar 17))
          sixBinderProvider = TypeForall [0 .. 5] [] token
          sevenBinderProvider = TypeForall [0 .. 6] [] token
      candidateProviderInstantiations [selected] sixBinderProvider @?=
        [ GroundProviderInstantiation
            { groundProviderArguments = replicate 6 selected
            , groundProviderType = token
            , groundProviderConstraints = []
            }
        ]
      assignmentProviderInstantiations [exactArguments]
          sixBinderProvider @?=
        [ GroundProviderInstantiation
            { groundProviderArguments = exactArguments
            , groundProviderType = token
            , groundProviderConstraints = []
            }
        ]
      candidateProviderInstantiations [selected] sevenBinderProvider @?= []
      assignmentProviderInstantiations
          [exactArguments ++ [seventhArgument]] sevenBinderProvider @?= []
  , testCase "generic deconstructors need no persistent flexible IDs" $ do
      let integer = TypeCons $ name "Int"
          box argument = TypeApp (TypeCons $ name "Box") argument
          deconstructor = DeconstructorBinding
            (box $ TypeVar 0)
            [ConstructorBinding (name "Box") [TypeVar 0]]
            False
          input = identityInput
            { E.input_goalType = TypeArrow (box integer) integer
            , E.input_envDeconsS = [deconstructor]
            }
          capacities = IdentifierCapacities 100 0 100 100
          isBoxElimination candidate = case candidate of
            ( ExpLambda scrutinee _
                (ExpLetMatch constructor [(field, annotation)]
                  (ExpVar matchedScrutinee _)
                  (ExpVar returnedField _))
              , []
              , _
              ) -> constructor == name "Box"
                && matchedScrutinee == scrutinee
                && returnedField == field
                && annotation == integer
            _ -> False
      chunks <- expectRight
        $ findExpressionsWithIdentifierCapacitiesEither capacities input
      finalChunk <- lastChunk "generic Box elimination" chunks
      E.chunkStatus finalChunk @?= E.SearchStatus E.SearchExhausted 0 0
      assertBool "generic Box elimination consumed a flexible ID"
        $ any isBoxElimination $ concatMap E.chunkElements chunks
  , testCase "multi-case deconstructors need no persistent flexible IDs" $ do
      let integer = TypeCons $ name "Int"
          choice argument = TypeApp (TypeCons $ name "Choice") argument
          genericChoice = choice $ TypeVar 0
          integerChoice = choice integer
          leftName = name "First"
          rightName = name "Second"
          deconstructor = DeconstructorBinding genericChoice
            [ ConstructorBinding leftName [TypeVar 0]
            , ConstructorBinding rightName [TypeVar 0]
            ] False
          input = identityInput
            { E.input_goalType = TypeArrow integerChoice integer
            , E.input_envDeconsS = [deconstructor]
            , E.input_multiPM = True
            , E.input_maxSteps = 200
            }
          capacities = IdentifierCapacities 100 0 100 100
          returnsAnnotatedField (_, [(field, annotation)], body) =
            annotation == integer && body == ExpVar field annotation
          returnsAnnotatedField _ = False
          isChoiceElimination candidate = case candidate of
            ( ExpLambda scrutinee _
                (ExpCaseMatch (ExpVar matchedScrutinee _) alternatives)
              , []
              , _
              ) -> matchedScrutinee == scrutinee
                && map (\(constructor, _, _) -> constructor) alternatives
                  == [leftName, rightName]
                && all returnsAnnotatedField alternatives
            _ -> False
      chunks <- expectRight
        $ findExpressionsWithIdentifierCapacitiesEither capacities input
      finalChunk <- lastChunk "generic Choice elimination" chunks
      E.chunkStatus finalChunk @?= E.SearchStatus E.SearchExhausted 0 0
      assertBool "generic Choice elimination consumed a flexible ID"
        $ any isChoiceElimination $ concatMap E.chunkElements chunks
  , testCase "empty elimination consumes no flexible identifier" $ do
      let integer = TypeCons $ name "Int"
          empty argument = TypeApp (TypeCons $ name "Empty") argument
          deconstructor = DeconstructorBinding
            (empty $ TypeVar 0) [] False
          input = identityInput
            { E.input_goalType = TypeArrow (empty integer) integer
            , E.input_envDeconsS = [deconstructor]
            }
      chunks <- expectRight
        $ findExpressionsWithIdentifierCapacitiesEither
            (IdentifierCapacities 100 0 100 100) input
      finalChunk <- lastChunk "empty elimination" chunks
      E.chunkStatus finalChunk @?= E.SearchStatus E.SearchExhausted 0 0
      assertBool "empty elimination required a non-escaping flexible ID"
        $ not $ null $ concatMap E.chunkElements chunks
  , testCase "empty deconstructors do not suppress provider use" $ do
      let integer = TypeCons $ name "Int"
          monomorphic = TypeArrow integer integer
          polymorphic = TypeForall [0] []
            $ TypeArrow (TypeVar 0) (TypeVar 0)
          emptyInteger = DeconstructorBinding integer [] False
          assertProviderRemainsUsable provider = do
            let input = identityInput
                  { E.input_goalType = TypeArrow provider
                      $ TypeArrow integer integer
                  , E.input_envDeconsS = [emptyInteger]
                  , E.input_maxSteps = 1024
                  }
            chunks <- expectRight
              $ findExpressionsWithIdentifierCapacitiesEither
                  (IdentifierCapacities 100 100 100 100) input
            -- With unused parameters forbidden, every surviving term must use
            -- both the provider and the Int argument. The eager empty-case
            -- branch alone therefore cannot make this assertion pass.
            assertBool
              ("constructorless Int suppressed provider " ++ show provider)
              $ not $ null $ concatMap E.chunkElements chunks
      mapM_ assertProviderRemainsUsable [monomorphic, polymorphic]
  , testCase "scope identifier collisions are operational truncations" $ do
      chunk <- lastCapacityChunk
        (IdentifierCapacities 100 100 100 1) identityInput
      E.chunkStatus chunk @?=
        E.SearchStatus E.SearchIdentifierSpaceExhausted 0 0
  , testCase "exact progress retains simultaneous step and ID limits" $ do
      let integer = TypeCons $ name "Int"
          boolean = TypeCons $ name "Bool"
          polymorphic = FunctionBinding
            (TypeVar 0) (name "polymorphic") 0 [] []
          deferred = FunctionBinding boolean (name "deferred") 0 [] [integer]
          input = identityInput
            { E.input_goalType = boolean
            , E.input_envFuncs = [polymorphic, deferred]
            -- The root opens even an empty leading forall before the
            -- binding-expansion step under test.
            , E.input_maxSteps = 2
            }
          capacities = IdentifierCapacities 100 0 100 100
      chunk <- lastCapacityChunk capacities input
      E.searchCompletion (E.chunkStatus chunk) @?=
        E.SearchIdentifierSpaceExhausted
      target <- checkedIdentifierTarget "generated"
      environment <- expectRight $ sealLegacyEnvironment input
      results <- expectRight
        $ findQueryResultsWithIdentifierCapacitiesEither
            capacities target
            (emptyExferenceSourceTypeVariableHints $ E.input_goalType input)
            environment (legacyInputQuery input)
      result <- lastResult results
      let batch = SharedQuery.resultSearch result
      SharedQuery.resultEvidence result @?= SharedQuery.NoEvidence
      SharedSearch.batchProgress batch @?= SharedSearch.Completed
        (SharedSearch.Truncated
          $ SharedSearch.StepLimitReached
            :| [SharedSearch.IdentifierSpaceExhausted])
  , testCase "queue representation overflow retains the best priorities" $ do
      let queued = [(2, 20)]
          generated = [(3, 30), (1, 10)]
      mergePriorityQueueAtCapacity 3 Nothing queued generated @?=
        ([(3, 30), (2, 20), (1, 10)], 0)
      mergePriorityQueueAtCapacity 2 Nothing queued generated @?=
        ([(3, 30), (2, 20)], 1)
      mergePriorityQueueAtCapacity 2 (Just 1) queued generated @?=
        ([(3, 30)], 2)
      mergePriorityQueueAtCapacity 3 (Just (-1)) queued generated @?=
        ([], 3)
  , testCase "compatibility pruning counts saturate without losing reasons" $ do
      let maximumCount = fromIntegral (maxBound :: Int) :: Natural
          queueTotal = maximumCount + 1
          depthTotal = maximumCount + 2
          binding = name "usedBinding"
      compatibilityPruningCount (maximumCount - 1) @?= maxBound - 1
      compatibilityPruningCount maximumCount @?= maxBound
      compatibilityPruningCount queueTotal @?= maxBound
      compatibilityBindingUsageCounts
          (Map.singleton binding queueTotal) @?=
        Map.singleton binding maxBound
      pruningReasonsFromNaturalTotals queueTotal depthTotal @?=
        [ SharedSearch.QueueLimitPruned queueTotal
        , SharedSearch.DepthLimitPruned depthTotal
        ]
  , testCase "structural tuple ranking exactly preserves applications" $ do
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
          complexity config = typeComplexityForTesting config
      assertEqual "fractional accumulation order"
        (complexity fractional legacy)
        (complexity fractional structural)
      assertEqual "near-saturation accumulation order"
        (complexity nearSaturation legacy)
        (complexity nearSaturation structural)
  , testCase "checker seals exact context-free visible specialization evidence" $ do
      let integer = TypeCons $ name "Int"
          provider = TypeForall [0] []
            $ TypeArrow (TypeVar 0) (TypeVar 0)
          result = TypeArrow integer integer
          goal = TypeArrow provider result
      argument <- expectRight $ Generated.specifiedVisibleTypeArgument integer
      let expression = ExpLambda 1 provider
            $ ExpTypeApply (ExpVar 1 provider) argument
      evidence <- checkedEvidence emptyStaticClassEnv [] [] goal expression
      null (checkedExpressionTypeApplicationOrigins evidence) @?= True
      checkedExpressionTypeApplicationOriginReferences evidence @?= []
      let availability = checkedExpressionTermGraph 23 evidence
      availability @?= checkedExpressionTermGraph 23 evidence
      case availability of
        ExferenceTermGraphUnavailable reason -> fail
          $ "context-free visible application had no typed graph: "
          ++ show reason
        ExferenceTermGraphAvailable graph -> do
          Typed.eraseTermGraph graph @?=
            Generated.discardUnusedPatternBindingsBy id
              (toGeneratedExpression expression)
          Typed.typedGraphVisibleTypeApplications
              (Typed.termGraphMetrics graph) @?= 1
          let witnesses =
                [ witness
                | (_, Typed.TermNode _
                    (Typed.TypedVisibleTypeApplication _ _ _ witness)) <-
                      Typed.termGraphNodes graph
                ]
          witnesses @?=
            [Typed.TypeApplicationWitness provider integer result Nothing]
          case checkedExpressionTermGraph 24 evidence of
            ExferenceTermGraphAvailable distinctGraph -> assertBool
              "different candidate keys reused one term-node identity"
              $ Typed.termGraphRoot distinctGraph /= Typed.termGraphRoot graph
            ExferenceTermGraphUnavailable reason -> fail
              $ "changing only the candidate key lost the graph: " ++ show reason
  , testCase "checker retains exact direct-global specialization origins" $ do
      let integer = TypeCons $ name "Int"
          boolean = TypeCons $ name "Bool"
          providerName = name "specialize"
          provider = TypeForall [0, 1] []
            $ TypeArrow (TypeVar 0)
            $ TypeArrow (TypeVar 1) (TypeVar 0)
          firstResult = TypeForall [1] []
            $ TypeArrow integer
            $ TypeArrow (TypeVar 1) integer
          result = TypeArrow integer $ TypeArrow boolean integer
      (bindings, schemes) <- preparedValueEnvironment providerName provider
      integerArgument <- expectRight
        $ Generated.specifiedVisibleTypeArgument integer
      booleanArgument <- expectRight
        $ Generated.specifiedVisibleTypeArgument boolean
      let expression = ExpTypeApply
            (ExpTypeApply (ExpName providerName) integerArgument)
            booleanArgument
      evidence <- checkedEvidenceWithSchemes emptyStaticClassEnv
        bindings [] schemes result expression
      checkedExpressionTypeApplicationOriginReferences evidence @?=
        [(0, 0), (0, 1)]
      case checkedExpressionTypeApplicationOrigins evidence of
        [origin] -> do
          checkedTypeApplicationOriginId origin @?= 0
          checkedTypeApplicationOriginOwner origin @?= providerName
          checkedTypeApplicationOriginSource origin @?= provider
          case checkedTypeApplicationOriginSteps origin of
            [first, second] -> do
              checkedTypeApplicationOriginStepSlot first @?= 0
              checkedTypeApplicationOriginStepSource first @?= provider
              checkedTypeApplicationOriginStepSelected first @?= integer
              checkedTypeApplicationOriginStepResult first @?= firstResult
              checkedTypeApplicationOriginStepObligations first @?= []
              checkedTypeApplicationOriginStepSlot second @?= 1
              checkedTypeApplicationOriginStepSource second @?= firstResult
              checkedTypeApplicationOriginStepSelected second @?= boolean
              checkedTypeApplicationOriginStepResult second @?= result
              checkedTypeApplicationOriginStepObligations second @?= []
            steps -> fail $ "unexpected retained specialization steps: "
              ++ show (length steps)
        origins -> fail $ "unexpected direct-global origin count: "
          ++ show (length origins)
      case checkedExpressionTermGraph 41 evidence of
        ExferenceTermGraphUnavailable reason -> fail
          $ "origin-bearing specialization lost its graph: " ++ show reason
        ExferenceTermGraphAvailable graph -> do
          let witnesses =
                [ witness
                | (_, Typed.TermNode _
                    (Typed.TypedVisibleTypeApplication _ _ _ witness)) <-
                      Typed.termGraphNodes graph
                ]
          length witnesses @?= 2
          map Typed.typeApplicationCertificate witnesses @?=
            [Nothing, Nothing]
  , testCase "specialization origins retain activated source constraints" $ do
      let className = name "C"
          providerName = name "contextual"
          integer = TypeCons $ name "Int"
          token = TypeCons $ name "Token"
          constraint ty = HsConstraint className [ty]
          provider = TypeForall [0] [constraint $ TypeVar 0] token
      (bindings, schemes) <- preparedValueEnvironment providerName provider
      classes <- expectRight $ mkStaticClassEnv
        [HsTypeClass className [0] []]
        [HsInstance [] $ constraint integer]
      argument <- expectRight
        $ Generated.specifiedVisibleTypeArgument integer
      let expression = ExpTypeApply (ExpName providerName) argument
      evidence <- checkedEvidenceWithSchemes classes bindings [] schemes
        token expression
      case checkedExpressionTypeApplicationOrigins evidence of
        [origin] -> case checkedTypeApplicationOriginSteps origin of
          [step] ->
            checkedTypeApplicationOriginStepObligations step @?=
              [constraint integer]
          steps -> fail $ "unexpected contextual step count: "
            ++ show (length steps)
        origins -> fail $ "unexpected contextual origin count: "
          ++ show (length origins)
      checkedExpressionTypeApplicationOriginReferences evidence @?= [(0, 0)]
      case checkedExpressionTermGraph 42 evidence of
        ExferenceTermGraphUnavailable
            (UnsupportedContextualVisibleApplication source selected actual) -> do
          source @?= provider
          selected @?= integer
          actual @?= token
        ExferenceTermGraphUnavailable reason -> fail
          $ "contextual origin changed graph absence: " ++ show reason
        ExferenceTermGraphAvailable _ -> fail
          "contextual origin unexpectedly made its graph available"
  , testCase "source constraints activate before a selected polytype suffix" $ do
      let className = name "C"
          providerName = name "polyContextual"
          integer = TypeCons $ name "Int"
          selected = TypeForall [7] [] $ TypeVar 7
          constraint ty = HsConstraint className [ty]
          provider = TypeForall [0] [constraint $ TypeVar 0] $ TypeVar 0
      (bindings, schemes) <- preparedValueEnvironment providerName provider
      classes <- expectRight $ mkStaticClassEnv
        [HsTypeClass className [0] []]
        [HsInstance [] $ constraint selected]
      selectedArgument <- expectRight
        $ Generated.specifiedVisibleTypeArgument selected
      integerArgument <- expectRight
        $ Generated.specifiedVisibleTypeArgument integer
      let expression = ExpTypeApply
            (ExpTypeApply (ExpName providerName) selectedArgument)
            integerArgument
      evidence <- checkedEvidenceWithSchemes classes bindings [] schemes
        integer expression
      checkedExpressionTypeApplicationOriginReferences evidence @?= [(0, 0)]
      case checkedExpressionTypeApplicationOrigins evidence of
        [origin] -> case checkedTypeApplicationOriginSteps origin of
          [step] -> do
            assertBool "selected polytype result changed alpha-structure"
              $ SharedTypeAtom.alphaEquivalentTypes
                  (checkedTypeApplicationOriginStepResult step) selected
            case checkedTypeApplicationOriginStepObligations step of
              [HsConstraint retainedClass [retainedSelected]] -> do
                retainedClass @?= className
                assertBool "source obligation lost the selected polytype"
                  $ SharedTypeAtom.alphaEquivalentTypes
                      retainedSelected selected
              obligations -> fail $ "unexpected selected-polytype obligations: "
                ++ show obligations
          steps -> fail $ "returned-polytype source step count changed: "
            ++ show (length steps)
        origins -> fail $ "returned-polytype origin count changed: "
          ++ show (length origins)

      -- The same source step must activate before any suffix exists.  This
      -- pins the checker correction which decides continuation from the raw
      -- source body, never from a replacement-introduced forall.
      noSuffix <- checkedEvidenceWithSchemes classes bindings [] schemes
        (TypeArrow integer selected)
        (ExpLambda 20 integer
          $ ExpTypeApply (ExpName providerName) selectedArgument)
      checkedExpressionTypeApplicationOriginReferences noSuffix @?= [(0, 0)]
      case checkedExpressionTypeApplicationOrigins noSuffix of
        [origin] -> case checkedTypeApplicationOriginSteps origin of
          [step] -> do
            assertBool "no-suffix selected result changed alpha-structure"
              $ SharedTypeAtom.alphaEquivalentTypes
                  (checkedTypeApplicationOriginStepResult step) selected
            case checkedTypeApplicationOriginStepObligations step of
              [HsConstraint retainedClass [retainedSelected]] -> do
                retainedClass @?= className
                assertBool "no-suffix source obligation changed selection"
                  $ SharedTypeAtom.alphaEquivalentTypes
                      retainedSelected selected
              obligations -> fail $ "unexpected no-suffix obligations: "
                ++ show obligations
          steps -> fail $ "unexpected no-suffix step count: "
            ++ show (length steps)
        origins -> fail $ "unexpected no-suffix origin count: "
          ++ show (length origins)
  , testCase "incomplete and compatibility visible spines stay origin-free" $ do
      let integer = TypeCons $ name "Int"
          boolean = TypeCons $ name "Bool"
          providerName = name "boundedSpecialize"
          provider = TypeForall [0, 1] []
            $ TypeArrow (TypeVar 0) (TypeVar 1)
      (bindings, schemes) <- preparedValueEnvironment providerName provider
      integerArgument <- expectRight
        $ Generated.specifiedVisibleTypeArgument integer
      booleanArgument <- expectRight
        $ Generated.specifiedVisibleTypeArgument boolean
      let remaining = TypeForall [1] [] $ TypeArrow integer $ TypeVar 1
          partialExpression = ExpTypeApply
            (ExpName providerName) integerArgument
      partial <- checkedEvidenceWithSchemes emptyStaticClassEnv bindings []
        schemes (TypeArrow integer remaining)
        (ExpLambda 20 integer partialExpression)
      null (checkedExpressionTypeApplicationOrigins partial) @?= True
      checkedExpressionTypeApplicationOriginReferences partial @?= []
      inferred <- checkedEvidenceWithSchemes emptyStaticClassEnv bindings []
        schemes (TypeArrow integer boolean)
        (ExpTypeApply
          (ExpTypeApply (ExpName providerName)
            Generated.inferredVisibleTypeArgument)
          booleanArgument)
      null (checkedExpressionTypeApplicationOrigins inferred) @?= True
      let fallbackName = name "compatibilityPolytype"
          fallbackScheme = TypeForall [0] [] $ TypeVar 0
          fallbackBinding = FunctionBinding fallbackScheme fallbackName 0 [] []
      compatibility <- checkedEvidence emptyStaticClassEnv [fallbackBinding] []
        integer (ExpTypeApply (ExpName fallbackName) integerArgument)
      null (checkedExpressionTypeApplicationOrigins compatibility) @?= True
  , testCase "origin eligibility is closed bounded and post-constraint" $ do
      let integer = TypeCons $ name "Int"
          boolean = TypeCons $ name "Bool"
          token = TypeCons $ name "Token"
          className = name "C"
          constraint ty = HsConstraint className [ty]
          specified ty = expectRight
            $ Generated.specifiedVisibleTypeArgument ty
      arguments <- mapM specified
        [ TypeCons $ name ("T" ++ show index)
        | index <- [0 :: Int .. 6]
        ]

      let sixName = name "sixOrigin"
          sixScheme = TypeForall [0 .. 5] [] token
      (sixBindings, sixSchemes) <- preparedValueEnvironment sixName sixScheme
      sixEvidence <- checkedEvidenceWithSchemes emptyStaticClassEnv sixBindings []
        sixSchemes token
        (foldl ExpTypeApply (ExpName sixName) $ take 6 arguments)
      length (checkedExpressionTypeApplicationOrigins sixEvidence) @?= 1
      checkedExpressionTypeApplicationOriginReferences sixEvidence @?=
        [(0, slot) | slot <- [0 .. 5]]

      let sevenName = name "sevenOrigin"
          sevenScheme = TypeForall [0 .. 6] [] token
      (sevenBindings, sevenSchemes) <- preparedValueEnvironment
        sevenName sevenScheme
      sevenEvidence <- checkedEvidenceWithSchemes emptyStaticClassEnv
        sevenBindings [] sevenSchemes token
        (foldl ExpTypeApply (ExpName sevenName) arguments)
      null (checkedExpressionTypeApplicationOrigins sevenEvidence) @?= True
      checkedExpressionTypeApplicationOriginReferences sevenEvidence @?= []

      -- A deliberately open retained scheme can satisfy the structural
      -- sidecar check, but cannot own a closed specialization origin.
      let openName = name "openOrigin"
          openScheme = TypeForall [0] []
            $ TypeArrow (TypeVar 0) (TypeVar 9)
      (openBindings, openSchemes) <- preparedValueEnvironment
        openName openScheme
      integerArgument <- specified integer
      openEvidence <- checkedEvidenceWithSchemes emptyStaticClassEnv
        openBindings [] openSchemes (TypeArrow integer $ TypeVar 9)
        (ExpTypeApply (ExpName openName) integerArgument)
      null (checkedExpressionTypeApplicationOrigins openEvidence) @?= True

      constrainedClasses <- expectRight $ mkStaticClassEnv
        [HsTypeClass className [0] []]
        [HsInstance [] $ constraint integer]
      let constrainedName = name "constraintGate"
          constrainedScheme = TypeForall [0]
            [constraint $ TypeVar 0] token
      (constrainedBindings, constrainedSchemes) <- preparedValueEnvironment
        constrainedName constrainedScheme
      booleanArgument <- specified boolean
      plan <- expectRight $ planRigidInstantiation
        (mkRigidInstantiationContext
          $ EnvDictionary constrainedBindings [] constrainedClasses)
        [] token
      context <- expectRight $ prepareExpressionCheckContextWithSchemes plan
        (mkQueryClassEnv constrainedClasses []) constrainedBindings []
        constrainedSchemes token
      case checkExpressionInContextWithNestedRigidProvenanceEvidence
          context (nestedRigidProvenance emptyRigidScope) []
          (ExpTypeApply (ExpName constrainedName) booleanArgument) of
        Left failure -> failure @?=
          ConstraintMismatch [] [constraint boolean]
        Right _ -> fail "an unsatisfied origin escaped the constraint gate"
  , testCase "failed preferred inference rolls origin state back" $ do
      let integer = TypeCons $ name "Int"
          providerName = name "transactionalOrigin"
          provider = TypeForall [0] []
            $ TypeArrow (TypeVar 0) (TypeVar 0)
          selected = TypeArrow integer integer
          nestedExpected = TypeForall [9] [] selected
      (bindings, schemes) <- preparedValueEnvironment providerName provider
      argument <- expectRight
        $ Generated.specifiedVisibleTypeArgument integer
      let visible = ExpTypeApply (ExpName providerName) argument
          expression = ExpLambda 20 integer visible
      evidence <- checkedEvidenceWithSchemes emptyStaticClassEnv bindings []
        schemes (TypeArrow integer nestedExpected) expression
      case checkedExpressionTypeApplicationOrigins evidence of
        [origin] -> do
          checkedTypeApplicationOriginId origin @?= 0
          checkedTypeApplicationOriginOwner origin @?= providerName
          map checkedTypeApplicationOriginStepSlot
            (checkedTypeApplicationOriginSteps origin) @?= [0]
        origins -> fail $ "transactional retry retained ghost origins: "
          ++ show (length origins)
      -- Forall introduction intentionally makes the graph draft unavailable,
      -- so there is no retained checked-term chain for the reference observer.
      checkedExpressionTypeApplicationOriginReferences evidence @?= []
  , testCase "checker retains only exact recursive zero-step spine cases" $ do
      let payload = TypeCons $ name "Payload"
          spineName = name "Spine"
          zeroName = name "SpineZero"
          stepName = name "SpineStep"
          spine = TypeApp (TypeCons spineName) payload
          zero = FunctionBinding spine zeroName 0 [] []
          step = FunctionBinding spine stepName 0 [] [payload, spine]
          exactDeconstructor = DeconstructorBinding spine
            [ ConstructorBinding zeroName []
            , ConstructorBinding stepName [payload, spine]
            ] True
          ordinaryDeconstructor = exactDeconstructor
            { deconstructorRecursive = False }
          goal = TypeArrow spine spine
          expression = ExpLambda 1 spine
            $ ExpCaseMatch (ExpVar 1 spine)
                [ (zeroName, [], ExpName zeroName)
                , ( stepName
                  , [(2, payload), (3, spine)]
                  , ExpVar 3 spine
                  )
                ]
          functions = [zero, step]
      evidence <- checkedEvidence emptyStaticClassEnv functions
        [exactDeconstructor] goal expression
      case checkedExpressionTermGraph 29 evidence of
        ExferenceTermGraphUnavailable reason -> fail
          $ "exact zero-step spine case had no typed graph: " ++ show reason
        ExferenceTermGraphAvailable graph -> do
          Typed.eraseTermGraph graph @?=
            Generated.discardUnusedPatternBindingsBy id
              (toGeneratedExpression expression)
          Typed.typedGraphCases (Typed.termGraphMetrics graph) @?= 1
          case
              [ alternatives
              | (_, Typed.TermNode _ (Typed.TypedCase _ alternatives)) <-
                  Typed.termGraphNodes graph
              ] of
            [[(_, _), (stepPattern, _)]] -> case
                Typed.typedPatternNode stepPattern of
              Typed.TypedConstructor retainedName [payloadPattern, tailPattern] -> do
                retainedName @?= stepName
                Typed.typedPatternNode payloadPattern @?= Typed.TypedWildcard
                case Typed.typedPatternNode tailPattern of
                  Typed.TypedBind{} -> pure ()
                  actual -> fail $ "recursive field lost its binder: "
                    ++ show actual
              actual -> fail $ "unexpected retained step pattern: " ++ show actual
            actual -> fail $ "unexpected retained case graph: " ++ show actual

      ordinaryEvidence <- checkedEvidence emptyStaticClassEnv functions
        [ordinaryDeconstructor] goal expression
      expectUnavailable "nonrecursive zero-step case"
        (== NominalConstructorPattern zeroName) ordinaryEvidence

      -- Shared generated-expression validation forbids one local identity
      -- from being rebound anywhere in a candidate. Keep that stronger raw
      -- boundary explicit even though checked-term free-local collection is
      -- independently lexical for lambda, let, and nested case scopes.
      let duplicate = 2
          duplicateError = Left $ InvalidCheckExpressionScope
            $ Generated.DuplicatePatternBinder duplicate
          stepFields = [(duplicate, payload), (3, spine)]
          withStepBody body = ExpLambda 1 spine
            $ ExpCaseMatch (ExpVar 1 spine)
                [ (zeroName, [], ExpName zeroName)
                , (stepName, stepFields, body)
                ]
          lambdaShadow = withStepBody
            $ ExpLambda duplicate payload $ ExpName zeroName
          letShadow = withStepBody
            $ ExpLet duplicate spine (ExpName zeroName)
                (ExpVar duplicate spine)
          nestedCaseShadow = withStepBody
            $ ExpCaseMatch (ExpVar 3 spine)
                [ (zeroName, [], ExpName zeroName)
                , (stepName, [(duplicate, payload), (4, spine)]
                    , ExpVar 4 spine)
                ]
      mapM_ (\shadowed -> checkExpression
          (mkQueryClassEnv emptyStaticClassEnv []) functions
          [exactDeconstructor] goal [] shadowed @?= duplicateError)
        [lambdaShadow, letShadow, nestedCaseShadow]

  , testCase "strict production search retains an exact zero-step case graph" $ do
      let payload = TypeCons $ name "Payload"
          spineName = name "Spine"
          zeroName = name "SpineZero"
          stepName = name "SpineStep"
          spine = TypeApp (TypeCons spineName) payload
          functions =
            [ FunctionBinding spine zeroName 0 [] []
            , FunctionBinding spine stepName 0 [] [payload, spine]
            ]
          deconstructor = DeconstructorBinding spine
            [ ConstructorBinding zeroName []
            , ConstructorBinding stepName [payload, spine]
            ] True
          input = identityInput
            { E.input_goalType = TypeArrow spine spine
            , E.input_envFuncs = functions
            , E.input_envDeconsS = [deconstructor]
            , E.input_allowUnused = False
            , E.input_multiPM = True
            , E.input_maxSteps = 500
            }
          capacities = IdentifierCapacities 100 100 100 100
          isRebuildCase (expression, _, availability) = case expression of
            ExpLambda scrutinee _
                (ExpCaseMatch (ExpVar matched _) alternatives)
              | scrutinee == matched
              , map (\(constructor, _, _) -> constructor) alternatives
                  == [zeroName, stepName]
              , [([], ExpName retainedZero),
                  ([(payloadVariable, payloadType),
                    (tailVariable, tailType)],
                    ExpApply
                      (ExpApply (ExpName retainedStep)
                        (ExpVar returnedPayload returnedPayloadType))
                      (ExpVar returnedTail returnedTailType))] <-
                    [ (fields, body)
                    | (_, fields, body) <- alternatives
                    ]
              , retainedZero == zeroName
              , retainedStep == stepName
              , payloadVariable == returnedPayload
              , payloadType == payload
              , returnedPayloadType == payload
              , tailVariable == returnedTail
              , tailType == spine
              , returnedTailType == spine -> case availability of
                  ExferenceTermGraphAvailable{} -> True
                  ExferenceTermGraphUnavailable{} -> False
            _ -> False
      candidates <- expectRight
        $ findTypedEngineCandidatesWithIdentifierCapacitiesEither
            capacities input
      assertBool "strict production search did not retain the rebuild-case graph"
        $ any isRebuildCase candidates
  , testCase "final zonking seals a multi-binder inferred specialization" $ do
      let integer = TypeCons $ name "Int"
          boolean = TypeCons $ name "Bool"
          provider = TypeForall [0, 1] []
            $ TypeArrow (TypeVar 0)
            $ TypeArrow (TypeVar 1) (TypeVar 0)
          expectedRemaining = TypeForall [7] []
            $ TypeArrow integer
            $ TypeArrow (TypeVar 7) integer
          expectedResult = TypeArrow integer
            $ TypeArrow boolean integer
      booleanArgument <- expectRight
        $ Generated.specifiedVisibleTypeArgument boolean
      let expression = ExpLambda 2 provider
            $ ExpTypeApply
                (ExpTypeApply (ExpVar 2 provider)
                  Generated.inferredVisibleTypeArgument)
                booleanArgument
      evidence <- checkedEvidence emptyStaticClassEnv [] []
        (TypeArrow provider expectedResult) expression
      case checkedExpressionTermGraph 31 evidence of
        ExferenceTermGraphUnavailable reason -> fail
          $ "multi-binder specialization had no typed graph: " ++ show reason
        ExferenceTermGraphAvailable graph -> do
          let witnesses =
                [ witness
                | (_, Typed.TermNode _
                    (Typed.TypedVisibleTypeApplication _ _ _ witness)) <-
                      Typed.termGraphNodes graph
                ]
              firstWitnesses =
                [ witness
                | witness <- witnesses
                , SharedTypeAtom.alphaEquivalentTypes
                    (Typed.typeApplicationSource witness) provider
                ]
              secondWitnesses =
                [ witness
                | witness <- witnesses
                , SharedTypeAtom.alphaEquivalentTypes
                    (Typed.typeApplicationSource witness) expectedRemaining
                ]
          case firstWitnesses of
            [witness] -> do
              Typed.typeApplicationSelected witness @?= integer
              assertBool "first visible result lost its remaining binder"
                $ SharedTypeAtom.alphaEquivalentTypes
                    (Typed.typeApplicationResult witness) expectedRemaining
            _ -> fail "missing unique first multi-binder witness"
          case secondWitnesses of
            [witness] -> do
              Typed.typeApplicationSelected witness @?= boolean
              Typed.typeApplicationResult witness @?= expectedResult
            _ -> fail "missing unique second multi-binder witness"
          assertBool "a final visible witness was not a leading instantiation"
            $ all
                (\witness -> SharedTypeAtom.isLeadingForallInstantiation
                  (Typed.typeApplicationSource witness)
                  (Typed.typeApplicationSelected witness)
                  (Typed.typeApplicationResult witness))
                witnesses
  , testCase "engine batches retain stable candidate-to-graph keying" $ do
      let integer = TypeCons $ name "Int"
          leftName = name "engineLeft"
          rightName = name "engineRight"
          binding bindingName = FunctionBinding integer bindingName 0 [] []
          input = identityInput
            { E.input_goalType = integer
            , E.input_envFuncs = [binding leftName, binding rightName]
            , E.input_maxSteps = 20
            }
          capacities = IdentifierCapacities 100 100 100 100
          observe candidates = Map.fromList
            [ (bindingName, Typed.termNodeIdValue $ Typed.termGraphRoot graph)
            | (ExpName bindingName, _, ExferenceTermGraphAvailable graph) <-
                candidates
            ]
      firstRun <- expectRight
        $ findTypedEngineCandidatesWithIdentifierCapacitiesEither
            capacities input
      secondRun <- expectRight
        $ findTypedEngineCandidatesWithIdentifierCapacitiesEither
            capacities input
      let first = observe firstRun
          second = observe secondRun
      Map.keysSet first @?= Set.fromList [leftName, rightName]
      first @?= second
      assertBool "two retained candidates reused one graph root identity"
        $ Map.lookup leftName first /= Map.lookup rightName first
  , testCase "engine batches retain impredicative multi-binder graphs" $ do
      let selected = TypeForall [2] []
            $ TypeArrow (TypeVar 2) (TypeVar 2)
          selectedPair = TypeTuple Boxed [selected, selected]
          provider = TypeForall [0, 1] [] selectedPair
          input = identityInput
            { E.input_goalType = TypeArrow provider selectedPair
            , E.input_envFuncs = []
            , E.input_maxSteps = 3
            }
          capacities = IdentifierCapacities 100 100 100 100
          visibleWitnesses graph =
            [ witness
            | (_, Typed.TermNode _
                (Typed.TypedVisibleTypeApplication _ _ _ witness)) <-
                  Typed.termGraphNodes graph
            ]
      argument <- expectRight
        $ Generated.specifiedVisibleTypeArgument selected
      let expectedExpression = ExpLambda 1 provider
            $ ExpTypeApply
                (ExpTypeApply (ExpVar 1 provider) argument) argument
      candidates <- expectRight
        $ findTypedEngineCandidatesWithIdentifierCapacitiesEither
            capacities input
      let impredicativeGraphs =
            [ (graph, witnesses)
            | (expression, _, ExferenceTermGraphAvailable graph) <- candidates
            , expression == expectedExpression
            , let witnesses = visibleWitnesses graph
            , length witnesses == 2
            , all
                (\witness -> SharedTypeAtom.alphaEquivalentTypes
                  (Typed.typeApplicationSelected witness) selected)
                witnesses
            ]
      case impredicativeGraphs of
        [] -> fail
          "production search retained no available impredicative multi-binder graph"
        [(graph, witnesses)] -> do
          Typed.typedGraphVisibleTypeApplications
              (Typed.termGraphMetrics graph) @?= 2
          assertBool "production witness chain is not leading-forall exact"
            $ all
                (\witness -> SharedTypeAtom.isLeadingForallInstantiation
                  (Typed.typeApplicationSource witness)
                  (Typed.typeApplicationSelected witness)
                  (Typed.typeApplicationResult witness))
                witnesses
        _ -> fail
          "production search duplicated the exact impredicative candidate"
  , testCase "typed evidence explains the deliberately unsupported subset" $ do
      let unit = TypeTuple Boxed []
          polymorphic = TypeForall [0] [] $ TypeVar 0
          implicitExpression = ExpLambda 1 polymorphic
            $ ExpVar 1 $ TypeVar 2
      implicitEvidence <- checkedEvidence emptyStaticClassEnv [] []
        (TypeArrow polymorphic unit) implicitExpression
      expectUnavailable "implicit local specialization"
        (\reason -> case reason of
          ImplicitLocalSpecialization{} -> True
          _ -> False)
        implicitEvidence

      let distinct = TypeForall [1] []
            $ TypeArrow (TypeVar 1) (TypeVar 1)
          subsumedExpression = ExpLambda 1 polymorphic $ ExpVar 1 distinct
      subsumedEvidence <- checkedEvidence emptyStaticClassEnv [] []
        (TypeArrow polymorphic distinct) subsumedExpression
      expectUnavailable "shallow local subsumption"
        (\reason -> case reason of
          SubsumedLocalSpecialization{} -> True
          _ -> False)
        subsumedEvidence

      let outer = TypeVar 4
          identityScheme = TypeForall [2] []
            $ TypeArrow (TypeVar 2) (TypeVar 2)
          rigidIdentity rigid local = ExpLambda local (TypeConstant rigid)
            $ ExpVar local $ TypeConstant rigid
          introducedExpression = ExpLambda 1 (TypeConstant 0)
            $ rigidIdentity 1 2
      introducedEvidence <- checkedEvidence emptyStaticClassEnv [] []
        (TypeArrow outer identityScheme) introducedExpression
      expectUnavailable "nested forall introduction"
        (\reason -> case reason of
          NestedForallIntroduction{} -> True
          _ -> False)
        introducedEvidence

      let integer = TypeCons $ name "Int"
          boxName = name "Box"
          box argument = TypeApp (TypeCons boxName) argument
          boxDeconstructor = DeconstructorBinding (box $ TypeVar 0)
            [ConstructorBinding boxName [TypeVar 0]] False
          matchedExpression = ExpLambda 1 (box integer)
            $ ExpLetMatch boxName [(2, integer)]
                (ExpVar 1 $ box integer)
                (ExpVar 2 integer)
      patternEvidence <- checkedEvidence emptyStaticClassEnv []
        [boxDeconstructor] (TypeArrow (box integer) integer)
        matchedExpression
      expectUnavailable "nominal constructor pattern"
        (\reason -> case reason of
          NominalConstructorPattern{} -> True
          _ -> False)
        patternEvidence

      pairName <- expectRight $ SharedName.tupleName Boxed 2
      let pairType = TypeTuple Boxed [integer, integer]
          tupleExpression = ExpLambda 1 pairType
            $ ExpLetMatch pairName [(2, integer), (3, integer)]
                (ExpVar 1 pairType)
                (ExpVar 2 integer)
      tupleEvidence <- checkedEvidence emptyStaticClassEnv [] []
        (TypeArrow pairType integer) tupleExpression
      expectUnavailable "structural tuple pattern"
        (\reason -> case reason of
          UnsupportedStructuralConstructorPattern{} -> True
          _ -> False)
        tupleEvidence

      let className = name "C"
          token = TypeCons $ name "Token"
          constraint ty = HsConstraint className [ty]
          contextualProvider = TypeForall [0]
            [constraint $ TypeVar 0] token
      contextualClasses <- expectRight $ mkStaticClassEnv
        [HsTypeClass className [0] []]
        [HsInstance [] $ constraint integer]
      integerArgument <- expectRight
        $ Generated.specifiedVisibleTypeArgument integer
      let contextualExpression = ExpLambda 1 contextualProvider
            $ ExpTypeApply (ExpVar 1 contextualProvider) integerArgument
      contextualEvidence <- checkedEvidence contextualClasses [] []
        (TypeArrow contextualProvider token) contextualExpression
      expectUnavailable "contextual visible application"
        (\reason -> case reason of
          UnsupportedContextualVisibleApplication{} -> True
          _ -> False)
        contextualEvidence

      let layeredContextualProvider = TypeForall [] [constraint integer]
            $ TypeForall [0, 1] []
            $ TypeArrow (TypeVar 0) (TypeVar 1)
          layeredContextualResult = TypeForall [] [constraint integer]
            $ TypeForall [1] []
            $ TypeArrow integer (TypeVar 1)
          layeredContextualExpression = ExpLambda 1 layeredContextualProvider
            $ ExpTypeApply
                (ExpVar 1 layeredContextualProvider) integerArgument
      layeredContextualEvidence <- checkedEvidence contextualClasses [] []
        (TypeArrow layeredContextualProvider layeredContextualResult)
        layeredContextualExpression
      case checkedExpressionTermGraph 37 layeredContextualEvidence of
        ExferenceTermGraphUnavailable
            (UnsupportedContextualVisibleApplication source selected result) -> do
            assertBool "layered source changed before classification"
              $ SharedTypeAtom.alphaEquivalentTypes
                  source layeredContextualProvider
            selected @?= integer
            assertBool "layered result lost its remaining binder"
              $ SharedTypeAtom.alphaEquivalentTypes
                  result layeredContextualResult
        ExferenceTermGraphUnavailable reason -> fail
          $ "layered contextual application had the wrong reason: "
          ++ show reason
        ExferenceTermGraphAvailable _ -> fail
          "layered contextual application unexpectedly produced a graph"
  , testCase "typed evidence reports sealing and projection mismatches" $ do
      let integer = TypeCons $ name "Int"
          seedName = name "seed"
          seed = FunctionBinding integer seedName 0 [] []
          oversizedExpression = foldr
            (\variable body -> ExpLet variable integer
              (ExpName seedName) body)
            (ExpName seedName)
            [1 .. 2050]
      oversizedEvidence <- checkedEvidence emptyStaticClassEnv [seed] []
        integer oversizedExpression
      expectUnavailable "graph node limit"
        (\reason -> case reason of
          TermGraphConstructionLimit
              (TermGraphConstructionNodeLimitExceeded 4096 4097) -> True
          _ -> False)
        oversizedEvidence

      let deepType = foldr TypeArrow integer $ replicate 2050 integer
          deepName = name "deep"
          deep = FunctionBinding deepType deepName 0 [] []
      deepEvidence <- checkedEvidence emptyStaticClassEnv [deep] []
        deepType $ ExpName deepName
      expectUnavailable "typed-graph type sealing"
        (\reason -> case reason of
          TermGraphSealingFailure
              (Typed.TermGraphTypeNodeLimitExceeded
                (Typed.GraphTermNodeType _) 4096 4097) -> True
          _ -> False)
        deepEvidence

      let unusedExpression = ExpLambda 1 integer $ ExpName seedName
      unusedEvidence <- checkedEvidence emptyStaticClassEnv [seed] []
        (TypeArrow integer integer) unusedExpression
      expectUnavailable "unused-binder compatibility projection"
        (== TermGraphProjectionMismatch) unusedEvidence
  , testCase "query-result projection preserves its envelope lazily" $ do
      targetName <- expectRight $ SharedName.mkOperator "<~>"
      target <- expectRight $ Generated.mkDefinitionName targetName
      let metadata = E.ExferenceBatchMetadata Map.empty 2 3
          (typedHasValidatedEvidence, typedProgress, typedMetadata,
            typedTarget, typedFallbackObserved) =
              typedQueryProjectionStrictnessForTesting target Map.empty
      typedHasValidatedEvidence @?= True
      typedProgress @?= SharedSearch.Continuing
      typedMetadata @?= metadata
      typedTarget @?= target
      typedFallbackObserved @?= True
      let
          (hasValidatedEvidence, progress, observedMetadata,
            observedTarget) =
              queryProjectionStrictnessForTesting target Map.empty
      hasValidatedEvidence @?= True
      progress @?= SharedSearch.Continuing
      observedMetadata @?= metadata
      observedTarget @?= target
      let (compatibilityStatus, compatibilityExpression,
            compatibilityConstraints, compatibilityStatistics) =
              compatibilityProjectionStrictnessForTesting
          compatibilityExpected = ExpLambda 1 (TypeVar 0)
            $ ExpVar 1 $ TypeVar 0
      compatibilityStatus @?=
        E.SearchStatus E.SearchRunning 2 3
      assertBool "compatibility projection changed the candidate expression"
        $ compatibilityExpression == compatibilityExpected
      compatibilityConstraints @?= []
      compatibilityStatistics @?= ExferenceStats 1 0 0
  ]

identityInput :: E.ExferenceInput
identityInput = E.ExferenceInput
  (TypeArrow (TypeVar 0) (TypeVar 0))
  [] [] emptyStaticClassEnv
  False False 0 False 20 Nothing Nothing defaultHeuristicsConfig

legacyInputEnvironment :: E.ExferenceInput -> EnvDictionary
legacyInputEnvironment input = EnvDictionary
  { environmentFunctions = E.input_envFuncs input
  , environmentDeconstructors = E.input_envDeconsS input
  , environmentClasses = E.input_envClasses input
  }

legacyInputQuery :: E.ExferenceInput -> E.ExferenceQuery
legacyInputQuery input = E.ExferenceQuery
  { E.queryGoalType = E.input_goalType input
  , E.queryExcludedBindings = Set.empty
  , E.querySearchOptions = ExferenceOptions
      { exferenceAllowUnused = E.input_allowUnused input
      , exferenceAllowResidualConstraints = E.input_allowConstraints input
      , exferenceConstraintDeferralSteps =
          E.input_allowConstraintsStopStep input
      , exferenceMultiConstructorPatterns = E.input_multiPM input
      , exferenceMaximumSteps = E.input_maxSteps input
      , exferenceMaximumQueueSize = E.input_maxQueueSize input
      , exferenceMaximumDepth = E.input_maxDepth input
      , exferenceHeuristics = E.input_heuristicsConfig input
      }
  }

sealLegacyEnvironment
  :: E.ExferenceInput
  -> Either E.ExferenceInputError E.ExferenceEnvironment
sealLegacyEnvironment = E.mkExferenceEnvironment . legacyInputEnvironment

lastCapacityChunk
  :: IdentifierCapacities
  -> E.ExferenceInput
  -> IO E.ExferenceChunkElement
lastCapacityChunk capacities input = do
  chunks <- expectRight
    $ findExpressionsWithIdentifierCapacitiesEither capacities input
  lastChunk "capacity-limited search" chunks

lastChunk :: String -> [value] -> IO value
lastChunk description chunks = case chunks of
  [] -> fail $ description ++ " produced no search chunks"
  first : remaining -> pure $ lastElement first remaining

lastResult :: [value] -> IO value
lastResult results = case results of
  [] -> fail "expected at least one capacity-limited query result"
  first : remaining -> pure $ lastElement first remaining

lastElement :: value -> [value] -> value
lastElement latest [] = latest
lastElement _ (next : remaining) = lastElement next remaining

checkedEvidence
  :: StaticClassEnv
  -> [FunctionBinding]
  -> [DeconstructorBinding]
  -> HsType
  -> Expression
  -> IO CheckedExpressionEvidence
checkedEvidence classes functions deconstructors goal expression = do
  plan <- expectRight $ planRigidInstantiation
    (mkRigidInstantiationContext
      $ EnvDictionary functions deconstructors classes)
    [] goal
  context <- expectRight $ prepareExpressionCheckContext plan
    (mkQueryClassEnv classes []) functions deconstructors goal
  expectRight $ checkExpressionInContextWithNestedRigidProvenanceEvidence
    context (nestedRigidProvenance emptyRigidScope) [] expression

checkedEvidenceWithSchemes
  :: StaticClassEnv
  -> [FunctionBinding]
  -> [DeconstructorBinding]
  -> Map.Map QualifiedName HsType
  -> HsType
  -> Expression
  -> IO CheckedExpressionEvidence
checkedEvidenceWithSchemes classes functions deconstructors schemes goal
    expression = do
  plan <- expectRight $ planRigidInstantiation
    (mkRigidInstantiationContext
      $ EnvDictionary functions deconstructors classes)
    [] goal
  context <- expectRight $ prepareExpressionCheckContextWithSchemes plan
    (mkQueryClassEnv classes []) functions deconstructors schemes goal
  expectRight $ checkExpressionInContextWithNestedRigidProvenanceEvidence
    context (nestedRigidProvenance emptyRigidScope) [] expression

preparedValueEnvironment
  :: QualifiedName
  -> HsType
  -> IO ([FunctionBinding], Map.Map QualifiedName HsType)
preparedValueEnvironment valueName valueType = do
  inventory <- expectRight $ SharedInventory.mkInventory
    SharedKindInference.OpenKindInventory
    ([ SharedDeclaration.ValueDeclaration
       $ SharedDeclaration.ValueSignature () valueName valueType
     ] :: [SharedDeclaration.Declaration SynthesisVariable Void ()])
  prepared <- expectRight $ prepareSynthesisInventory inventory
  let backend = preparedSynthesisBackend prepared
  pure
    ( environmentFunctions backend
    , preparedSynthesisSchemes prepared
    )

expectUnavailable
  :: String
  -> (ExferenceTermGraphAbsence -> Bool)
  -> CheckedExpressionEvidence
  -> IO ()
expectUnavailable label matches evidence =
  case checkedExpressionTermGraph 23 evidence of
    ExferenceTermGraphUnavailable reason
      | matches reason -> pure ()
      | otherwise -> fail $ label ++ ": wrong absence: " ++ show reason
    ExferenceTermGraphAvailable graph ->
      fail $ label ++ ": unexpectedly sealed: " ++ show graph

checkedIdentifierTarget :: String -> IO Generated.DefinitionName
checkedIdentifierTarget spelling = do
  targetName <- expectRight $ SharedName.mkIdentifier spelling
  expectRight $ Generated.mkDefinitionName targetName

name :: String -> QualifiedName
name spelling = either (error . show) id $ mkQualifiedName [] spelling

expectRight :: Show problem => Either problem result -> IO result
expectRight = either (fail . show) pure
