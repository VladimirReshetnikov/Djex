module Main (main) where

import Control.Exception (evaluate)
import Data.List (sort)
import System.Timeout (timeout)

import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Declaration
  ( Declaration (ClassDeclaration) )
import Language.Haskell.Synthesis.Inventory
  ( Inventory
  , mkInventory
  )
import Language.Haskell.Synthesis.KindInference
  ( KindInventoryPolicy (ClosedKindInventory) )
import Language.Haskell.Synthesis.Name
  ( Boxity (Boxed)
  , Name
  , listName
  , parseName
  )
import qualified Language.Haskell.Synthesis.Semantic.Length as Length
import Language.Haskell.Synthesis.Type (Type (..))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
  ( assertBool
  , assertFailure
  , testCase
  , (@?=)
  )

main :: IO ()
main = defaultMain lengthTests

lengthTests :: TestTree
lengthTests = testGroup "finite-list-spine-length/v1"
  [ limitTests
  , contractTests
  , providerTests
  , normalizationTests
  , productiveBoundTests
  , fingerprintTests
  ]

limitTests :: TestTree
limitTests = testGroup "limits"
  [ testCase "publish the exact versioned domain tag" $
      Length.finiteListSpineLengthDomainTag @?=
        map (fromIntegral . fromEnum)
          ("finite-list-spine-length/v1" :: String)
  , testCase "publish the intended conservative defaults" $ do
      Length.mkLengthLimits Length.defaultLengthLimitSource @?=
        Right Length.defaultLengthLimits
      let limits = Length.defaultLengthLimits
      Length.lengthTypeNodeLimit limits @?= 4096
      Length.lengthContractInputLimit limits @?= 8
      Length.lengthSyntaxNodeLimit limits @?= 1024
      Length.lengthFormulaClauseLimit limits @?= 32
      Length.lengthCollectionWidthLimit limits @?= 64
      Length.lengthProviderSummaryLimit limits @?= 256
      Length.lengthProviderArgumentLimit limits @?= 16
      Length.lengthLiteralBitLimit limits @?= 256
      Length.lengthFingerprintByteLimit limits @?= 65536
  , testCase "reject every negative field in declaration order" $
      mapM_ assertNegativeLimit negativeLimitCases
  , testCase "admit zero for every bound" $ do
      limits <- expectRight $ Length.mkLengthLimits zeroLengthLimitSource
      map ($ limits)
        [ Length.lengthTypeNodeLimit
        , Length.lengthContractInputLimit
        , Length.lengthSyntaxNodeLimit
        , Length.lengthFormulaClauseLimit
        , Length.lengthCollectionWidthLimit
        , Length.lengthProviderSummaryLimit
        , Length.lengthProviderArgumentLimit
        , Length.lengthLiteralBitLimit
        , Length.lengthFingerprintByteLimit
        ] @?= replicate 9 0
  ]

contractTests :: TestTree
contractTests = testGroup "checked contracts"
  [ testCase "admit opaque impredicative list payloads" $ do
      let payload = polymorphicIdentityType
          target = FunctionType (listOf payload) (listOf payload)
      checked <- expectRight $ sealContract
        Length.defaultLengthLimits target identityLengthContract
      Length.checkedLengthContractTarget checked @?= target
      Length.checkedLengthContractInputCount checked @?= 1
      Length.checkedLengthContractPrecondition checked @?=
        Length.LengthTruth True
      Length.checkedLengthContractPostcondition checked @?=
        Length.LengthEqual
          (Length.LengthVariable $ Length.LengthInput 0)
          (Length.LengthVariable Length.LengthResult)
  , testCase "reject a direct higher-rank term input" $ do
      let rankNInput = polymorphicIdentityType
          target = FunctionType rankNInput (listOf rankNInput)
      case sealContract
          Length.defaultLengthLimits target identityLengthContract of
        Left (Length.LengthContractInputIsNotList 0 rejected) ->
          rejected @?= rankNInput
        Left other -> assertFailure $ "unexpected rejection: " ++ show other
        Right _ -> assertFailure "direct higher-rank input was admitted as a list"
  , testCase "require proper-kind authority for opaque list payloads" $ do
      let illKindedTarget =
            listOf (TypeConstructor listName) :: Type String
      case sealContract Length.defaultLengthLimits
          illKindedTarget trivialLengthContract of
        Left Length.LengthContractTargetKindError{} -> pure ()
        Left other -> assertFailure $ "unexpected rejection: " ++ show other
        Right _ -> assertFailure "an ill-kinded list payload was admitted"
  , testCase "reject a proper-kinded leading contract context" $ do
      className <- expectName "Fixture.ContractConstraint"
      let declaration :: Declaration String () ()
          declaration = ClassDeclaration () className [] [] []
          target = ForallType [] [Constraint className []]
            $ listOf closedPayloadType
      inventory <- expectRight $ mkInventory
        ClosedKindInventory [declaration]
      assertLeft Length.LengthContractConstrainedTarget
        $ Length.sealLengthContract Length.defaultLengthLimits
            inventory target trivialLengthContract
  , testCase "reject result references in a precondition" $ do
      let source = Length.LengthContractSource
            { Length.lengthContractPrecondition = Length.LengthEqual
                (Length.LengthVariable Length.LengthResult)
                (Length.LengthLiteral 0)
            , Length.lengthContractPostcondition = Length.LengthTruth True
            }
      assertLeft
        (Length.LengthContractPreconditionError
          Length.LengthResultNotAvailableInPrecondition)
        $ sealContract Length.defaultLengthLimits
            (listOf closedPayloadType) source
  , testCase "reject input references outside the target function spine" $ do
      let source = Length.LengthContractSource
            { Length.lengthContractPrecondition = Length.LengthTruth True
            , Length.lengthContractPostcondition = Length.LengthEqual
                (Length.LengthVariable $ Length.LengthInput 1)
                (Length.LengthVariable Length.LengthResult)
            }
      assertLeft
        (Length.LengthContractPostconditionError
          $ Length.LengthInputReferenceOutOfRange 1 1)
        $ sealContract Length.defaultLengthLimits
            (FunctionType
              (listOf closedPayloadType)
              (listOf closedPayloadType)) source
  , testCase "report the first input beyond an exact zero bound" $ do
      let limits = limitsWith $ \source -> source
            { Length.lengthLimitSourceContractInputs = 0 }
      assertLeft (Length.LengthContractInputLimitExceeded 0 1)
        $ sealContract limits
            (FunctionType
              (listOf closedPayloadType)
              (listOf closedPayloadType)) identityLengthContract
  , testCase "bound raw syntax, conjunctions, widths, and literals" $ do
      let target = FunctionType
            (listOf closedPayloadType)
            (listOf closedPayloadType)
          syntaxLimits = limitsWith $ \source -> source
            { Length.lengthLimitSourceSyntaxNodes = 1 }
          syntaxSource = contractWith
            (Length.LengthNot $ Length.LengthTruth True)
            (Length.LengthTruth True)
          clauseLimits = limitsWith $ \source -> source
            { Length.lengthLimitSourceFormulaClauses = 1 }
          clauses = Length.LengthAll
            [Length.LengthTruth True, Length.LengthTruth False]
          widthLimits = limitsWith $ \source -> source
            { Length.lengthLimitSourceCollectionWidth = 1 }
          literalLimits = limitsWith $ \source -> source
            { Length.lengthLimitSourceLiteralBits = 3 }
          literalSource = contractWith
            (Length.LengthEqual
              (Length.LengthVariable $ Length.LengthInput 0)
              (Length.LengthLiteral 8))
            (Length.LengthTruth True)
          foldedSumSource = contractWith
            (Length.LengthEqual
              (Length.LengthSum
                [Length.LengthLiteral 7, Length.LengthLiteral 1])
              (Length.LengthLiteral 0))
            (Length.LengthTruth True)
          foldedScaleSource = contractWith
            (Length.LengthEqual
              (Length.LengthScale 2 $ Length.LengthLiteral 4)
              (Length.LengthLiteral 0))
            (Length.LengthTruth True)
      assertLeft
        (Length.LengthContractPreconditionError
          $ Length.LengthSyntaxNodeLimitExceeded 1 2)
        $ sealContract syntaxLimits target syntaxSource
      assertLeft
        (Length.LengthContractPreconditionError
          $ Length.LengthFormulaClauseLimitExceeded 1 2)
        $ sealContract clauseLimits target
            (contractWith clauses $ Length.LengthTruth True)
      assertLeft
        (Length.LengthContractPreconditionError
          $ Length.LengthSyntaxCollectionLimitExceeded
              Length.LengthConjunctionClauses 1 2)
        $ sealContract widthLimits target
            (contractWith clauses $ Length.LengthTruth True)
      assertLeft
        (Length.LengthContractPreconditionError
          $ Length.LengthLiteralBitLimitExceeded 3 4)
        $ sealContract literalLimits target literalSource
      assertLeft
        (Length.LengthContractPreconditionError
          $ Length.LengthLiteralBitLimitExceeded 3 4)
        $ sealContract literalLimits target foldedSumSource
      assertLeft
        (Length.LengthContractPreconditionError
          $ Length.LengthLiteralBitLimitExceeded 3 4)
        $ sealContract literalLimits target foldedScaleSource
  , testCase "bound target structure and type collections" $ do
      let nodeLimits = limitsWith $ \source -> source
            { Length.lengthLimitSourceTypeNodes = 0 }
          widthLimits = limitsWith $ \source -> source
            { Length.lengthLimitSourceCollectionWidth = 1 }
          wideTarget = ForallType ["left", "right"] []
            (listOf closedPayloadType)
      assertLeft
        (Length.LengthContractTargetBoundError
          $ Length.LengthTypeNodeLimitExceeded 0 1)
        $ sealContract nodeLimits
            (listOf closedPayloadType) trivialLengthContract
      assertLeft
        (Length.LengthContractTargetBoundError
          $ Length.LengthTypeCollectionLimitExceeded
              Length.LengthForallBinders 1 2)
        $ sealContract widthLimits wideTarget
            trivialLengthContract
  , testCase "surface a bounded fingerprint failure at max plus one" $ do
      let limits = limitsWith $ \source -> source
            { Length.lengthLimitSourceFingerprintBytes = 0 }
      assertLeft (Length.LengthContractFingerprintLimitExceeded 0 1)
        $ sealContract limits
            (listOf closedPayloadType) trivialLengthContract
  ]

providerTests :: TestTree
providerTests = testGroup "assumed provider inventory"
  [ testCase "retain closed schemes, roles, transfers, and assumed trust" $ do
      providerName <- expectName "Fixture.preserve"
      let source = unaryListProvider providerName
            Length.LengthSpineArgument
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
      inventory <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [source]
      case Length.lookupCheckedLengthProviderSummary providerName inventory of
        Nothing -> assertFailure "checked provider disappeared from inventory"
        Just checked -> do
          Length.checkedLengthProviderName checked @?= providerName
          Length.checkedLengthProviderScheme checked @?=
            Length.lengthProviderScheme source
          Length.checkedLengthProviderArgumentRoles checked @?=
            [Length.LengthSpineArgument]
          Length.checkedLengthProviderTransfer checked @?=
            Length.LengthVariable (Length.LengthProviderArgument 0)
          Length.checkedLengthProviderTrust checked @?=
            Length.AssumedProviderLaw
  , testCase "admit non-list arguments only when their spines are unobserved" $ do
      providerName <- expectName "Fixture.constant"
      let scheme = FunctionType closedPayloadType
            (listOf closedPayloadType)
          source = providerSource providerName scheme
            [Length.LengthUnobservedArgument]
            (Length.LengthLiteral 0)
      inventory <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [source]
      let summaries = Length.checkedLengthProviderSummaries inventory
      map Length.checkedLengthProviderArgumentRoles summaries @?=
        [[Length.LengthUnobservedArgument]]
  , testCase "admit rank-N unobserved arguments and impredicative list payloads" $ do
      unobservedName <- expectName "Fixture.rankNUnobserved"
      spineName <- expectName "Fixture.impredicativeSpine"
      let unobserved = providerSource unobservedName
            (FunctionType polymorphicIdentityType
              $ listOf closedPayloadType)
            [Length.LengthUnobservedArgument]
            (Length.LengthLiteral 0)
          impredicative = providerSource spineName
            (FunctionType
              (listOf polymorphicIdentityType)
              (listOf polymorphicIdentityType))
            [Length.LengthSpineArgument]
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
      inventory <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [unobserved, impredicative]
      map Length.checkedLengthProviderName
          (Length.checkedLengthProviderSummaries inventory) @?=
        sort [unobservedName, spineName]
  , testCase "require proper-kind authority for unobserved provider arguments" $ do
      providerName <- expectName "Fixture.illKinded"
      let source = providerSource providerName
            (FunctionType
              (TypeConstructor listName)
              (listOf closedPayloadType))
            [Length.LengthUnobservedArgument]
            (Length.LengthLiteral 0)
      case sealProviderInventory Length.defaultLengthLimits [source] of
        Left (Length.LengthProviderSummaryRejected 0 name
            Length.LengthProviderSchemeKindError{}) ->
          name @?= providerName
        Left other -> assertFailure $ "unexpected rejection: " ++ show other
        Right _ -> assertFailure "an ill-kinded provider scheme was admitted"
  , testCase "reject open provider schemes" $ do
      providerName <- expectName "Fixture.open"
      let openScheme = FunctionType
            (listOf $ TypeVariable "free")
            (listOf $ TypeVariable "free")
          source = providerSource providerName openScheme
            [Length.LengthSpineArgument]
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          $ Length.LengthProviderOpenScheme ["free"])
        $ sealProviderInventory
            Length.defaultLengthLimits [source]
  , testCase "reject a proper-kinded leading provider context" $ do
      className <- expectName "Fixture.ProviderConstraint"
      providerName <- expectName "Fixture.constrained"
      let declaration :: Declaration String () ()
          declaration = ClassDeclaration () className [] [] []
          scheme = ForallType [] [Constraint className []]
            $ FunctionType
                (listOf closedPayloadType)
                (listOf closedPayloadType)
          source = providerSource providerName scheme
            [Length.LengthSpineArgument]
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
      inventory <- expectRight $ mkInventory
        ClosedKindInventory [declaration]
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          Length.LengthProviderConstrainedScheme)
        $ Length.sealLengthProviderInventory
            Length.defaultLengthLimits inventory [source]
  , testCase "reject direct rank-N spines and observed non-list arguments" $ do
      rankNName <- expectName "Fixture.rankN"
      nonListName <- expectName "Fixture.nonList"
      let rankNScheme = FunctionType polymorphicIdentityType
            (listOf closedPayloadType)
          rankNSource = providerSource rankNName rankNScheme
            [Length.LengthSpineArgument]
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
          nonListScheme = FunctionType closedPayloadType
            (listOf closedPayloadType)
          nonListSource = providerSource nonListName nonListScheme
            [Length.LengthSpineArgument]
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
      case sealProviderInventory
          Length.defaultLengthLimits [rankNSource] of
        Left (Length.LengthProviderSummaryRejected 0 name
            (Length.LengthProviderSpineArgumentIsNotList 0 _)) ->
          name @?= rankNName
        Left other -> assertFailure $ "unexpected rejection: " ++ show other
        Right _ -> assertFailure "direct higher-rank provider input was admitted"
      assertLeft
        (Length.LengthProviderSummaryRejected 0 nonListName
          $ Length.LengthProviderSpineArgumentIsNotList 0 closedPayloadType)
        $ sealProviderInventory
            Length.defaultLengthLimits [nonListSource]
  , testCase "reject transfer references to absent or unobserved arguments" $ do
      providerName <- expectName "Fixture.badTransfer"
      let scheme = FunctionType closedPayloadType
            (listOf closedPayloadType)
          outOfRange = providerSource providerName scheme
            [Length.LengthUnobservedArgument]
            (Length.LengthVariable $ Length.LengthProviderArgument 1)
          unobserved = providerSource providerName scheme
            [Length.LengthUnobservedArgument]
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          $ Length.LengthProviderTransferError
          $ Length.LengthProviderReferenceOutOfRange 1 1)
        $ sealProviderInventory
            Length.defaultLengthLimits [outOfRange]
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          $ Length.LengthProviderTransferError
          $ Length.LengthProviderReferenceIsUnobserved 0)
        $ sealProviderInventory
            Length.defaultLengthLimits [unobserved]
  , testCase "reject role arity mismatches and duplicate names" $ do
      providerName <- expectName "Fixture.duplicate"
      let source = unaryListProvider providerName
            Length.LengthSpineArgument
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
          missingRole = source { Length.lengthProviderArgumentRoles = [] }
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          $ Length.LengthProviderRoleArityMismatch 1 0)
        $ sealProviderInventory
            Length.defaultLengthLimits [missingRole]
      assertLeft (Length.DuplicateLengthProvider providerName)
        $ sealProviderInventory
            Length.defaultLengthLimits [source, source]
      assertLeft (Length.DuplicateLengthProvider providerName)
        $ sealProviderInventory Length.defaultLengthLimits
            [ source
            , source { Length.lengthProviderArgumentRoles = [] }
            ]
  , testCase "canonicalize source order without losing deterministic name order" $ do
      alpha <- expectName "Fixture.alpha"
      beta <- expectName "Fixture.beta"
      let first = unaryListProvider alpha Length.LengthSpineArgument
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
          second = unaryListProvider beta Length.LengthSpineArgument
            (Length.LengthScale 2
              $ Length.LengthVariable $ Length.LengthProviderArgument 0)
      forward <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [first, second]
      reverseOrder <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [second, first]
      Length.lengthProviderInventoryFingerprint forward @?=
        Length.lengthProviderInventoryFingerprint reverseOrder
      map Length.checkedLengthProviderName
          (Length.checkedLengthProviderSummaries forward) @?=
        sort [alpha, beta]
      map Length.checkedLengthProviderName
          (Length.checkedLengthProviderSummaries reverseOrder) @?=
        sort [alpha, beta]
  , testCase "enforce provider count, argument count, and fingerprint bounds" $ do
      providerName <- expectName "Fixture.bounded"
      let source = unaryListProvider providerName
            Length.LengthSpineArgument
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
          noSummaries = limitsWith $ \limits -> limits
            { Length.lengthLimitSourceProviderSummaries = 0 }
          noArguments = limitsWith $ \limits -> limits
            { Length.lengthLimitSourceProviderArguments = 0 }
          noFingerprint = limitsWith $ \limits -> limits
            { Length.lengthLimitSourceFingerprintBytes = 0 }
      assertLeft (Length.LengthProviderSummaryLimitExceeded 0 1)
        $ sealProviderInventory noSummaries [source]
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          $ Length.LengthProviderArgumentLimitExceeded 0 1)
        $ sealProviderInventory noArguments [source]
      assertLeft
        (Length.LengthProviderInventoryFingerprintLimitExceeded 0 1)
        $ sealProviderInventory noFingerprint
            ([] :: [Length.LengthProviderSummarySource String])
  ]

normalizationTests :: TestTree
normalizationTests = testGroup "normalization"
  [ testCase "make conjunction and additive permutations idempotent" $ do
      let target = FunctionType
            (listOf closedPayloadType)
            (listOf closedPayloadType)
          input = Length.LengthVariable $ Length.LengthInput 0
          result = Length.LengthVariable Length.LengthResult
          lowerBound = Length.LengthAtMost (Length.LengthLiteral 1) input
          fixedPoint = Length.LengthEqual input input
          leftSource = contractWith
            (Length.LengthAll
              [ fixedPoint
              , lowerBound
              , fixedPoint
              , Length.LengthTruth True
              ])
            (Length.LengthEqual result
              $ Length.LengthSum
                  [input, Length.LengthLiteral 0, Length.LengthLiteral 1])
          rightSource = contractWith
            (Length.LengthAll
              [ Length.LengthAll [lowerBound, fixedPoint]
              , fixedPoint
              ])
            (Length.LengthEqual
              (Length.LengthSum
                [Length.LengthLiteral 1, input, Length.LengthLiteral 0])
              result)
      left <- expectRight $ sealContract
        Length.defaultLengthLimits target leftSource
      right <- expectRight $ sealContract
        Length.defaultLengthLimits target rightSource
      Length.checkedLengthContractPrecondition left @?=
        Length.checkedLengthContractPrecondition right
      Length.checkedLengthContractPostcondition left @?=
        Length.checkedLengthContractPostcondition right
      Length.lengthContractFingerprint left @?=
        Length.lengthContractFingerprint right
      resealed <- expectRight $ sealContract
        Length.defaultLengthLimits
        (Length.checkedLengthContractTarget left)
        Length.LengthContractSource
          { Length.lengthContractPrecondition =
              Length.checkedLengthContractPrecondition left
          , Length.lengthContractPostcondition =
              Length.checkedLengthContractPostcondition left
          }
      Length.checkedLengthContractPrecondition resealed @?=
        Length.checkedLengthContractPrecondition left
      Length.checkedLengthContractPostcondition resealed @?=
        Length.checkedLengthContractPostcondition left
      Length.lengthContractFingerprint resealed @?=
        Length.lengthContractFingerprint left
  , testCase "keep alpha-renamed impredicative payloads opaque" $ do
      let firstPayload = ForallType ["a"] [] $ FunctionType
            (TypeVariable "a") (TypeVariable "a")
          secondPayload = ForallType ["renamed"] [] $ FunctionType
            (TypeVariable "renamed") (TypeVariable "renamed")
      first <- expectRight $ sealContract
        Length.defaultLengthLimits
        (listOf firstPayload) trivialLengthContract
      second <- expectRight $ sealContract
        Length.defaultLengthLimits
        (listOf secondPayload) trivialLengthContract
      Length.lengthContractFingerprint first @?=
        Length.lengthContractFingerprint second
  , testCase "canonicalize minimum and maximum association and order" $ do
      let target = FunctionType
            (listOf closedPayloadType)
            (listOf closedPayloadType)
          input = Length.LengthVariable $ Length.LengthInput 0
          result = Length.LengthVariable Length.LengthResult
          firstSource = contractWith (Length.LengthTruth True)
            (Length.LengthAll
              [ Length.LengthEqual result
                  (Length.LengthMinimum input
                    $ Length.LengthMinimum
                        (Length.LengthLiteral 3) result)
              , Length.LengthEqual input
                  (Length.LengthMaximum result
                    $ Length.LengthMaximum
                        (Length.LengthLiteral 2) input)
              ])
          reassociated = contractWith (Length.LengthTruth True)
            (Length.LengthAll
              [ Length.LengthEqual input
                  (Length.LengthMaximum
                    (Length.LengthMaximum input
                      $ Length.LengthLiteral 2)
                    result)
              , Length.LengthEqual result
                  (Length.LengthMinimum
                    (Length.LengthMinimum result input)
                    $ Length.LengthLiteral 3)
              ])
      first <- expectRight $ sealContract
        Length.defaultLengthLimits target firstSource
      second <- expectRight $ sealContract
        Length.defaultLengthLimits target reassociated
      Length.checkedLengthContractPostcondition first @?=
        Length.checkedLengthContractPostcondition second
      Length.lengthContractFingerprint first @?=
        Length.lengthContractFingerprint second
  ]

productiveBoundTests :: TestTree
productiveBoundTests = testGroup "productive bounded traversal"
  [ testCase "stop on a cyclic target at the first type node past the bound" $ do
      let limits = limitsWith $ \source -> source
            { Length.lengthLimitSourceTypeNodes = 4 }
          cyclicTarget = FunctionType
            (listOf closedPayloadType) cyclicTarget
      observed <- evaluateWithin $ sealContract
        limits cyclicTarget trivialLengthContract
      assertLeft
        (Length.LengthContractTargetBoundError
          $ Length.LengthTypeNodeLimitExceeded 4 5)
        observed
  , testCase "stop on a cyclic formula AST at the first node past the bound" $ do
      let limits = limitsWith $ \limitSource -> limitSource
            { Length.lengthLimitSourceSyntaxNodes = 3 }
          cyclicFormula = Length.LengthNot cyclicFormula
          source = contractWith cyclicFormula (Length.LengthTruth True)
      observed <- evaluateWithin $ sealContract
        limits (listOf closedPayloadType) source
      assertLeft
        (Length.LengthContractPreconditionError
          $ Length.LengthSyntaxNodeLimitExceeded 3 4)
        observed
  , testCase "stop on cyclic sum terms at collection width plus one" $ do
      let limits = limitsWith $ \limitSource -> limitSource
            { Length.lengthLimitSourceCollectionWidth = 1 }
          cyclicTerms = Length.LengthLiteral 0 : cyclicTerms
          source = contractWith
            (Length.LengthEqual
              (Length.LengthSum cyclicTerms)
              (Length.LengthLiteral 0))
            (Length.LengthTruth True)
      observed <- evaluateWithin $ sealContract
        limits (listOf closedPayloadType) source
      assertLeft
        (Length.LengthContractPreconditionError
          $ Length.LengthSyntaxCollectionLimitExceeded
              Length.LengthSumTerms 1 2)
        observed
  , testCase "stop on a cyclic provider source list at max plus one" $ do
      providerName <- expectName "Fixture.cyclic"
      let limits = limitsWith $ \source -> source
            { Length.lengthLimitSourceProviderSummaries = 1 }
          provider = unaryListProvider providerName
            Length.LengthSpineArgument
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
          cyclicProviders = provider : cyclicProviders
      observed <- evaluateWithin $ sealProviderInventory
        limits cyclicProviders
      assertLeft (Length.LengthProviderSummaryLimitExceeded 1 2) observed
  , testCase "stop on a cyclic role list at the argument bound" $ do
      providerName <- expectName "Fixture.cyclicRoles"
      let roles = Length.LengthSpineArgument : roles
          provider = (unaryListProvider providerName
            Length.LengthSpineArgument
            (Length.LengthVariable $ Length.LengthProviderArgument 0))
              { Length.lengthProviderArgumentRoles = roles }
      observed <- evaluateWithin $ sealProviderInventory
        Length.defaultLengthLimits [provider]
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          $ Length.LengthProviderArgumentLimitExceeded 16 17)
        observed
  , testCase "stop on a cyclic provider transfer at the syntax bound" $ do
      providerName <- expectName "Fixture.cyclicTransfer"
      let limits = limitsWith $ \source -> source
            { Length.lengthLimitSourceSyntaxNodes = 3 }
          transfer = Length.LengthScale 1 transfer
          provider = unaryListProvider providerName
            Length.LengthSpineArgument transfer
      observed <- evaluateWithin $ sealProviderInventory limits [provider]
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          $ Length.LengthProviderTransferError
          $ Length.LengthSyntaxNodeLimitExceeded 3 4)
        observed
  , testCase "validate semantically discarded provider subtrees" $ do
      providerName <- expectName "Fixture.discardedTransfer"
      let limits = limitsWith $ \source -> source
            { Length.lengthLimitSourceSyntaxNodes = 4 }
          cyclic = Length.LengthScale 1 cyclic
          conditional = unaryListProvider providerName
            Length.LengthSpineArgument
            (Length.LengthIf
              (Length.LengthTruth True)
              (Length.LengthLiteral 0)
              cyclic)
          scaledAway = unaryListProvider providerName
            Length.LengthSpineArgument
            (Length.LengthScale 0 cyclic)
      conditionalResult <- evaluateWithin
        $ sealProviderInventory limits [conditional]
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          $ Length.LengthProviderTransferError
          $ Length.LengthSyntaxNodeLimitExceeded 4 5)
        conditionalResult
      scaledResult <- evaluateWithin
        $ sealProviderInventory limits [scaledAway]
      assertLeft
        (Length.LengthProviderSummaryRejected 0 providerName
          $ Length.LengthProviderTransferError
          $ Length.LengthSyntaxNodeLimitExceeded 4 5)
        scaledResult
  ]

fingerprintTests :: TestTree
fingerprintTests = testGroup "identity sensitivity"
  [ testCase "abstract opaque payloads but distinguish behavioral formulas" $ do
      let firstTarget = listOf closedPayloadType
          secondTarget = listOf
            (FunctionType closedPayloadType closedPayloadType)
          stronger = contractWith (Length.LengthTruth True)
            (Length.LengthEqual
              (Length.LengthVariable Length.LengthResult)
              (Length.LengthLiteral 1))
      baseline <- expectRight $ sealContract
        Length.defaultLengthLimits firstTarget trivialLengthContract
      differentPayload <- expectRight $ sealContract
        Length.defaultLengthLimits secondTarget trivialLengthContract
      differentFormula <- expectRight $ sealContract
        Length.defaultLengthLimits firstTarget stronger
      assertBool "opaque payload leaked into length-contract identity" $
        Length.lengthContractFingerprint baseline ==
          Length.lengthContractFingerprint differentPayload
      assertBool "behavioral formula was omitted from contract identity" $
        Length.lengthContractFingerprint baseline /=
          Length.lengthContractFingerprint differentFormula
  , testCase "distinguish the ordered contract input spine" $ do
      nullary <- expectRight $ sealContract
        Length.defaultLengthLimits
        (listOf closedPayloadType) trivialLengthContract
      unary <- expectRight $ sealContract
        Length.defaultLengthLimits
        (FunctionType
          (listOf closedPayloadType)
          (listOf closedPayloadType))
        trivialLengthContract
      assertBool "contract input arity was omitted from identity" $
        Length.lengthContractFingerprint nullary /=
          Length.lengthContractFingerprint unary
  , testCase "exclude admission limits from contract and inventory identity" $ do
      providerName <- expectName "Fixture.limitIndependent"
      let tightLimits = limitsWith $ \source -> source
            { Length.lengthLimitSourceTypeNodes = 32
            , Length.lengthLimitSourceContractInputs = 1
            , Length.lengthLimitSourceSyntaxNodes = 16
            , Length.lengthLimitSourceFormulaClauses = 8
            , Length.lengthLimitSourceCollectionWidth = 8
            , Length.lengthLimitSourceProviderSummaries = 1
            , Length.lengthLimitSourceProviderArguments = 1
            , Length.lengthLimitSourceLiteralBits = 8
            , Length.lengthLimitSourceFingerprintBytes = 65535
            }
          target = FunctionType
            (listOf closedPayloadType)
            (listOf closedPayloadType)
          provider = unaryListProvider providerName
            Length.LengthSpineArgument
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
      defaultContract <- expectRight $ sealContract
        Length.defaultLengthLimits target identityLengthContract
      tightContract <- expectRight $ sealContract
        tightLimits target identityLengthContract
      defaultInventory <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [provider]
      tightInventory <- expectRight $ sealProviderInventory
        tightLimits [provider]
      Length.lengthContractFingerprint defaultContract @?=
        Length.lengthContractFingerprint tightContract
      Length.lengthProviderInventoryFingerprint defaultInventory @?=
        Length.lengthProviderInventoryFingerprint tightInventory
  , testCase "identify alpha-equivalent closed provider schemes" $ do
      providerName <- expectName "Fixture.alphaEquivalent"
      let scheme binder = ForallType [binder] [] $ FunctionType
            (listOf $ TypeVariable binder)
            (listOf $ TypeVariable binder)
          source binder = providerSource providerName (scheme binder)
            [Length.LengthSpineArgument]
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
      first <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [source "element"]
      renamed <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [source "renamed"]
      Length.lengthProviderInventoryFingerprint first @?=
        Length.lengthProviderInventoryFingerprint renamed
  , testCase "retain ordered provider argument roles in identity" $ do
      providerName <- expectName "Fixture.roles"
      let scheme = FunctionType
            (listOf closedPayloadType)
            (FunctionType
              (listOf closedPayloadType)
              (listOf closedPayloadType))
          source roles = providerSource providerName scheme roles
            (Length.LengthLiteral 0)
      first <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits
        [source
          [ Length.LengthSpineArgument
          , Length.LengthUnobservedArgument
          ]]
      swapped <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits
        [source
          [ Length.LengthUnobservedArgument
          , Length.LengthSpineArgument
          ]]
      assertBool "ordered provider roles were omitted from identity" $
        Length.lengthProviderInventoryFingerprint first /=
          Length.lengthProviderInventoryFingerprint swapped
  , testCase "distinguish provider name, scheme, and assumed transfer" $ do
      firstName <- expectName "Fixture.first"
      secondName <- expectName "Fixture.second"
      let identitySource = unaryListProvider firstName
            Length.LengthSpineArgument
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
          renamedSource = identitySource
            { Length.lengthProviderName = secondName }
          scaledSource = identitySource
            { Length.lengthProviderTransfer = Length.LengthScale 2
                $ Length.LengthVariable $ Length.LengthProviderArgument 0 }
          changedScheme = providerSource firstName
            (FunctionType
              (listOf closedPayloadType)
              (listOf $ FunctionType
                closedPayloadType closedPayloadType))
            [Length.LengthSpineArgument]
            (Length.LengthVariable $ Length.LengthProviderArgument 0)
      baseline <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [identitySource]
      renamed <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [renamedSource]
      scaled <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [scaledSource]
      changed <- expectRight $ sealProviderInventory
        Length.defaultLengthLimits [changedScheme]
      let baselineFingerprint =
            Length.lengthProviderInventoryFingerprint baseline
      assertBool "provider name was omitted from inventory identity" $
        baselineFingerprint /= Length.lengthProviderInventoryFingerprint renamed
      assertBool "provider transfer was omitted from inventory identity" $
        baselineFingerprint /= Length.lengthProviderInventoryFingerprint scaled
      assertBool "provider scheme was omitted from inventory identity" $
        baselineFingerprint /= Length.lengthProviderInventoryFingerprint changed
  ]

negativeLimitCases
  :: [(String, Length.LengthLimitField, Length.LengthLimitSource)]
negativeLimitCases =
  [ ("type nodes", Length.LengthTypeNodes, defaults
      { Length.lengthLimitSourceTypeNodes = -1 })
  , ("contract inputs", Length.LengthContractInputs, defaults
      { Length.lengthLimitSourceContractInputs = -1 })
  , ("syntax nodes", Length.LengthSyntaxNodes, defaults
      { Length.lengthLimitSourceSyntaxNodes = -1 })
  , ("formula clauses", Length.LengthFormulaClauses, defaults
      { Length.lengthLimitSourceFormulaClauses = -1 })
  , ("collection width", Length.LengthCollectionWidth, defaults
      { Length.lengthLimitSourceCollectionWidth = -1 })
  , ("provider summaries", Length.LengthProviderSummaries, defaults
      { Length.lengthLimitSourceProviderSummaries = -1 })
  , ("provider arguments", Length.LengthProviderArguments, defaults
      { Length.lengthLimitSourceProviderArguments = -1 })
  , ("literal bits", Length.LengthLiteralBits, defaults
      { Length.lengthLimitSourceLiteralBits = -1 })
  , ("fingerprint bytes", Length.LengthFingerprintBytes, defaults
      { Length.lengthLimitSourceFingerprintBytes = -1 })
  ]
 where
  defaults = Length.defaultLengthLimitSource

assertNegativeLimit
  :: (String, Length.LengthLimitField, Length.LengthLimitSource)
  -> IO ()
assertNegativeLimit (label, field, source) =
  case Length.mkLengthLimits source of
    Left failure -> failure @?= Length.NegativeLengthLimit field (-1)
    Right _ -> assertFailure $ "negative " ++ label ++ " limit was accepted"

zeroLengthLimitSource :: Length.LengthLimitSource
zeroLengthLimitSource = Length.LengthLimitSource
  { Length.lengthLimitSourceTypeNodes = 0
  , Length.lengthLimitSourceContractInputs = 0
  , Length.lengthLimitSourceSyntaxNodes = 0
  , Length.lengthLimitSourceFormulaClauses = 0
  , Length.lengthLimitSourceCollectionWidth = 0
  , Length.lengthLimitSourceProviderSummaries = 0
  , Length.lengthLimitSourceProviderArguments = 0
  , Length.lengthLimitSourceLiteralBits = 0
  , Length.lengthLimitSourceFingerprintBytes = 0
  }

limitsWith
  :: (Length.LengthLimitSource -> Length.LengthLimitSource)
  -> Length.LengthLimits
limitsWith transform = case Length.mkLengthLimits
    $ transform Length.defaultLengthLimitSource of
  Left failure -> error $ "invalid test limits: " ++ show failure
  Right limits -> limits

closedPayloadType :: Type String
closedPayloadType = TupleType Boxed []

polymorphicIdentityType :: Type String
polymorphicIdentityType = ForallType ["element"] [] $ FunctionType
  (TypeVariable "element") (TypeVariable "element")

listOf :: Type variable -> Type variable
listOf = TypeApplication $ TypeConstructor listName

identityLengthContract :: Length.LengthContractSource
identityLengthContract = contractWith
  (Length.LengthTruth True)
  (Length.LengthEqual
    (Length.LengthVariable Length.LengthResult)
    (Length.LengthVariable $ Length.LengthInput 0))

trivialLengthContract :: Length.LengthContractSource
trivialLengthContract = contractWith
  (Length.LengthTruth True) (Length.LengthTruth True)

contractWith
  :: Length.LengthFormula Length.LengthContractVariable
  -> Length.LengthFormula Length.LengthContractVariable
  -> Length.LengthContractSource
contractWith precondition postcondition = Length.LengthContractSource
  { Length.lengthContractPrecondition = precondition
  , Length.lengthContractPostcondition = postcondition
  }

fixtureInventory :: Inventory String ()
fixtureInventory = case mkInventory ClosedKindInventory
    ([] :: [Declaration String () ()]) of
  Left failure -> error $ "invalid fixture inventory: " ++ show failure
  Right inventory -> inventory

sealContract
  :: Length.LengthLimits
  -> Type String
  -> Length.LengthContractSource
  -> Either
      (Length.LengthContractError String)
      (Length.CheckedLengthContract String)
sealContract limits = Length.sealLengthContract limits fixtureInventory

sealProviderInventory
  :: Length.LengthLimits
  -> [Length.LengthProviderSummarySource String]
  -> Either
      (Length.LengthProviderInventoryError String)
      (Length.CheckedLengthProviderInventory String)
sealProviderInventory limits =
  Length.sealLengthProviderInventory limits fixtureInventory

providerSource
  :: Name
  -> Type String
  -> [Length.LengthProviderArgumentRole]
  -> Length.LengthExpression Length.LengthProviderVariable
  -> Length.LengthProviderSummarySource String
providerSource providerName scheme roles transfer =
  Length.AssumedProviderSummary
    { Length.lengthProviderName = providerName
    , Length.lengthProviderScheme = scheme
    , Length.lengthProviderArgumentRoles = roles
    , Length.lengthProviderTransfer = transfer
    }

unaryListProvider
  :: Name
  -> Length.LengthProviderArgumentRole
  -> Length.LengthExpression Length.LengthProviderVariable
  -> Length.LengthProviderSummarySource String
unaryListProvider providerName role transfer = providerSource providerName
  (ForallType ["element"] [] $ FunctionType
    (listOf $ TypeVariable "element")
    (listOf $ TypeVariable "element"))
  [role]
  transfer

expectName :: String -> IO Name
expectName = expectRight . parseName

expectRight :: Show error => Either error value -> IO value
expectRight result = case result of
  Left failure -> assertFailure $ "unexpected rejection: " ++ show failure
  Right value -> pure value

assertLeft
  :: (Eq error, Show error)
  => error
  -> Either error value
  -> IO ()
assertLeft expected result = case result of
  Left failure -> failure @?= expected
  Right _ -> assertFailure $ "expected rejection: " ++ show expected

evaluateWithin :: value -> IO value
evaluateWithin value = do
  observed <- timeout 2000000 $ evaluate value
  case observed of
    Nothing -> fail "bounded validation did not terminate within two seconds"
    Just result -> pure result
