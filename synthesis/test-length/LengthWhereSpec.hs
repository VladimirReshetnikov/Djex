{-# LANGUAGE TemplateHaskell #-}

module LengthWhereSpec (lengthWhereTests) where

import Data.Maybe (catMaybes)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (isInfixOf)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Unsafe.Coerce (unsafeCoerce)

import qualified Language.Haskell.Djex as Djex
import Language.Haskell.Synthesis.Declaration (Declaration)
import qualified Language.Haskell.Synthesis.Fingerprint as Fingerprint
import Language.Haskell.Synthesis.Inventory (Inventory, mkInventory)
import qualified Language.Haskell.Synthesis.Internal.TypedCandidate
  as InternalTypedCandidate
import Language.Haskell.Synthesis.KindInference
  ( KindInventoryPolicy (ClosedKindInventory) )
import Language.Haskell.Synthesis.Name (Boxity (Boxed), listName)
import qualified Language.Haskell.Synthesis.Semantic.Length as Length
import qualified Language.Haskell.Synthesis.Semantic.Length.Problem
  as LengthProblem
import qualified Language.Haskell.Synthesis.Semantic.Length.SMTLib as SMTLib
import qualified Language.Haskell.Synthesis.Semantic.Length.Where as Where
import Language.Haskell.Synthesis.Type (Type (..), Variable (..))
import qualified Language.Haskell.TH as TH
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion
  , assertBool
  , assertFailure
  , testCase
  , (@?=)
  )

lengthWhereSourceOpacityCharacterized :: ()
lengthWhereSourceOpacityCharacterized =
  $(do
      representation <- TH.reify ''Where.LengthWhereSource
      -- Reification makes this independent of hypothetical field spellings:
      -- any record constructor changes NormalC to RecC.  The namespace probe
      -- separately rejects an exported positional constructor, and instance
      -- reification sees both deriving clauses and standalone declarations.
      constructorName <- case representation of
        TH.TyConI (TH.DataD _ _ _ _ [TH.NormalC name _] _) -> pure name
        _ -> fail $ "LengthWhereSource is no longer one positional data " ++
          "constructor: " ++ show representation
      let constructorBase = TH.nameBase constructorName
      visibleConstructors <- traverse TH.lookupValueName
        [ "Where." ++ constructorBase
        , "Djex." ++ constructorBase
        ]
      case catMaybes visibleConstructors of
        [] -> pure ()
        visible -> fail $ "public LengthWhereSource constructors: " ++
          show visible
      let sourceType = TH.ConT ''Where.LengthWhereSource
          capabilities =
            [ ("Eq", ''Eq)
            , ("Ord", ''Ord)
            , ("Show", ''Show)
            , ("Generic", ''Generic)
            ]
      instances <- traverse (\(label, className) -> do
          declarations <- TH.reifyInstances className [sourceType]
          pure (label, declarations)) capabilities
      case [(label, declarations) | (label, declarations) <- instances,
              not $ null declarations] of
        [] -> [| () |]
        visible -> fail $ "public LengthWhereSource instances: " ++ show visible)

lengthWhereTests :: TestTree
lengthWhereTests = testGroup "bounded Length where syntax"
  [ relationTests
  , arithmeticTests
  , divisorTests
  , haskellSurfaceTests
  , leanSurfaceTests
  , roleTests
  , byteAndOffsetTests
  , resourceTests
  , refusalTests
  , identityTests
  , publicSurfaceTests
  ]

haskellSurfaceTests :: TestTree
haskellSurfaceTests = testGroup "Haskell-shaped surface"
  [ testCase "lower ordinary length application and Haskell relations" $ do
      assertHaskellScalar observedOne "length result == length arg0"
        $ Length.LengthEqual result input0
      assertHaskellScalar observedOne "length result /= length arg0"
        $ Length.LengthNot $ Length.LengthEqual result input0
      assertHaskellScalar observedOne "length result <= length arg0"
        $ Length.LengthAtMost result input0
      assertHaskellScalar observedOne "length result < length arg0"
        $ Length.LengthNot $ Length.LengthAtMost input0 result
      assertHaskellScalar observedOne "length result >= length arg0"
        $ Length.LengthAtMost input0 result
      assertHaskellScalar observedOne "length result > length arg0"
        $ Length.LengthNot $ Length.LengthAtMost result input0
  , testCase "lower Haskell arithmetic, div, mod, min, and max" $
      assertHaskellScalar observedThree
        ("length result == length arg0 + 2 * length arg1 " ++
          "- length arg2 `div` 3 `mod` 2")
        (Length.LengthEqual result
          $ Length.LengthMonus
              (Length.LengthSum
                [ input0
                , Length.LengthScale 2 input1
                ])
              (Length.LengthModulo 2 $ Length.LengthQuotient 3 input2))
  , testCase "lower prefix div and mod application" $
      assertHaskellScalar observedOne
        "length result == mod (div (length arg0) 3) 2"
        (Length.LengthEqual result
          $ Length.LengthModulo 2 $ Length.LengthQuotient 3 input0)
  , testCase "lower Haskell prefix extrema" $
      assertHaskellScalar observedTwo
        "length result == min (length arg1) (max (length arg0) 3)"
        (Length.LengthEqual result
          $ Length.LengthMinimum input1
              (Length.LengthMaximum input0 $ Length.LengthLiteral 3))
  , testCase "lower fst and snd result projections" $
      assertHaskellPair observedOne
        ("length (fst result) + length (snd result) " ++
          "== 2 * length arg0")
        (Length.LengthEqual
          (Length.LengthSum [pairFirst, pairSecond])
          (Length.LengthScale 2 pairInput0))
  , testCase "retain native offsets and reject cross-surface spellings" $ do
      assertHaskellParseError "length result = length arg0"
        $ Where.LengthWhereUnexpectedToken 14
            Where.LengthWhereRelationExpected
      assertHaskellParseError "length result != length arg0"
        $ Where.LengthWhereUnexpectedToken 14
            Where.LengthWhereRelationExpected
      assertHaskellParseError "len(result)==len(arg0)"
        $ Where.LengthWhereUnexpectedToken 0
            Where.LengthWhereExpressionExpected
      assertHaskellParseError "length (fst arg0) == length result"
        $ Where.LengthWhereUnexpectedToken 12
            Where.LengthWhereReferenceExpected
      assertParseError "length result == length arg0"
        $ Where.LengthWhereUnexpectedToken 0
            Where.LengthWhereExpressionExpected
      assertParseError "0/=0"
        $ Where.LengthWhereUnexpectedToken 2
            Where.LengthWhereExpressionExpected
  , testCase "require Haskell application grouping for compound arguments" $ do
      assertHaskellParseError "length result == min length arg0 1"
        $ Where.LengthWhereUnexpectedToken 21
            Where.LengthWhereExpressionExpected
      assertHaskellParseError "length result == min(length arg0,1)"
        $ Where.LengthWhereUnexpectedToken 32
            Where.LengthWhereRightParenthesisExpected
      assertHaskellParseError "length result == length arg0 div 2"
        $ Where.LengthWhereUnexpectedToken 29 Where.LengthWhereEndExpected
  , testCase "share exact byte, ASCII, and parenthesis bounds" $ do
      let exact = ascii $ "0==0" ++ replicate 16380 ' '
          excess = BS.snoc exact 0x20
          nested count = "length " ++ replicate count '(' ++ "arg0" ++
            replicate count ')' ++ " == 0"
      parsed <- case Where.parseHaskellLengthWhereSource defaultLimits exact of
        Left failure -> assertFailure (show failure) >> error "unreachable"
        Right value -> pure value
      assertSeals defaultLimits Where.LengthWhereScalar [] parsed
      case Where.parseHaskellLengthWhereSource defaultLimits excess of
        Left failure -> failure @?=
          Where.LengthWhereSourceByteLimitExceeded 16384 16385
        Right _ -> assertFailure "accepted oversized Haskell-shaped source"
      case Where.parseHaskellLengthWhereSource defaultLimits
          (BS.pack [0x30, 0x3d, 0x3d, 0x30, 0x20, 0x80, 0xff]) of
        Left failure -> failure @?= Where.LengthWhereNonAsciiByte 5
        Right _ -> assertFailure "accepted non-ASCII Haskell-shaped source"
      assertHaskellScalar observedOne (nested 64)
        $ Length.LengthEqual input0 (Length.LengthLiteral 0)
      assertHaskellParseError (nested 65)
        $ Where.LengthWhereSyntaxLimitExceeded
            Where.LengthWhereNestingDepth 64 65 71
  , testCase "share emitted semantic resource limits" $ do
      assertHaskellParseErrorUnder (limitsWithSyntax 2 32 64 256) "0/=0"
        $ Where.LengthWhereSyntaxLimitExceeded
            Where.LengthWhereSyntaxNodes 2 3 1
      assertHaskellParseErrorUnder (limitsWithSyntax 1024 1 64 256) "0==0"
        $ Where.LengthWhereSyntaxLimitExceeded
            Where.LengthWhereFormulaClauses 1 2 1
      assertHaskellParseErrorUnder (limitsWithSyntax 1024 32 1 256)
        "length arg0 + length arg1 == 0"
        $ Where.LengthWhereSyntaxLimitExceeded
            Where.LengthWhereCollectionWidth 1 2 12
      assertHaskellParseErrorUnder (limitsWithSyntax 1024 32 64 3) "8==0"
        $ Where.LengthWhereSyntaxLimitExceeded
            Where.LengthWhereLiteralBits 3 4 0
      assertHaskellParseError "length arg8 == 0"
        $ Where.LengthWhereSyntaxLimitExceeded
            Where.LengthWherePhysicalArgumentIndex 7 8 7
  , testCase "produce the same scalar and pair contract sources" $ do
      compactScalar <- scalarContract defaultLimits observedOne
        "len(result)=len(arg0)+min(len(arg0),1)"
      haskellScalar <- haskellScalarContract observedOne
        "length result == length arg0 + min (length arg0) 1"
      haskellScalar @?= compactScalar
      compactPair <- pairContract defaultLimits observedOne
        "len(result.first)+len(result.second)=2*len(arg0)"
      haskellPair <- haskellPairContract observedOne
        ("length (fst result) + length (snd result) " ++
          "== 2 * length arg0")
      haskellPair @?= compactPair
  , testCase "re-export the Haskell parser through the facade" $ do
      source <- case Djex.parseHaskellLengthWhereSource Djex.defaultLengthLimits
          (ascii "length result == 0") of
        Left failure -> assertFailure (show failure) >> error "unreachable"
        Right value -> pure value
      Djex.elaborateLengthWhereSource Djex.LengthWhereScalar [] source @?=
        Right (Djex.LengthWhereScalarContractSource []
          $ scalarSource $ Length.LengthEqual result
              (Length.LengthLiteral 0))
  ]

leanSurfaceTests :: TestTree
leanSurfaceTests = testGroup "Lean-shaped surface"
  [ testCase "lower List.length and Lean relations" $ do
      assertLeanScalar observedOne "List.length result = List.length arg0"
        $ Length.LengthEqual result input0
      assertLeanScalar observedOne "List.length result != List.length arg0"
        $ Length.LengthNot $ Length.LengthEqual result input0
      assertLeanScalar observedOne "List.length result <= List.length arg0"
        $ Length.LengthAtMost result input0
      assertLeanScalar observedOne "List.length result < List.length arg0"
        $ Length.LengthNot $ Length.LengthAtMost input0 result
      assertLeanScalar observedOne "List.length result >= List.length arg0"
        $ Length.LengthAtMost input0 result
      assertLeanScalar observedOne "List.length result > List.length arg0"
        $ Length.LengthNot $ Length.LengthAtMost result input0
  , testCase "lower Lean arithmetic and prefix extrema" $ do
      assertLeanScalar observedThree
        ("List.length result = List.length arg0 + 2 * List.length arg1 " ++
          "- List.length arg2 / 3 % 2")
        (Length.LengthEqual result
          $ Length.LengthMonus
              (Length.LengthSum
                [ input0
                , Length.LengthScale 2 input1
                ])
              (Length.LengthModulo 2 $ Length.LengthQuotient 3 input2))
      assertLeanScalar observedTwo
        ("List.length result = min (List.length arg1) " ++
          "(max (List.length arg0) 3)")
        (Length.LengthEqual result
          $ Length.LengthMinimum input1
              (Length.LengthMaximum input0 $ Length.LengthLiteral 3))
  , testCase "lower numeric product projections" $
      assertLeanPair observedOne
        ("List.length result.1 + List.length result.2 " ++
          "= 2 * List.length arg0")
        (Length.LengthEqual
          (Length.LengthSum [pairFirst, pairSecond])
          (Length.LengthScale 2 pairInput0))
  , testCase "admit parenthesized reference arguments" $ do
      assertLeanScalar observedOne
        "List.length (result) = List.length (arg0)"
        $ Length.LengthEqual result input0
      assertLeanPair [] "List.length (result.1) = List.length (result.2)"
        $ Length.LengthEqual pairFirst pairSecond
  , testCase "reject compact, Haskell, and malformed Lean spellings" $ do
      assertLeanParseError "len(result)=len(arg0)"
        $ Where.LengthWhereUnexpectedToken 0
            Where.LengthWhereExpressionExpected
      assertLeanParseError "length result == length arg0"
        $ Where.LengthWhereUnexpectedToken 0
            Where.LengthWhereExpressionExpected
      assertLeanParseError "List.length result == List.length arg0"
        $ Where.LengthWhereUnexpectedToken 19
            Where.LengthWhereRelationExpected
      assertLeanParseError "List.length result.first = 0"
        $ Where.LengthWhereUnexpectedToken 19
            Where.LengthWhereReferenceExpected
      assertLeanParseError "List.length result.3 = 0"
        $ Where.LengthWhereUnexpectedToken 19
            Where.LengthWhereReferenceExpected
  , testCase "share emitted resource limits and source offsets" $ do
      assertLeanParseErrorUnder (limitsWithSyntax 2 32 64 256) "0!=0"
        $ Where.LengthWhereSyntaxLimitExceeded
            Where.LengthWhereSyntaxNodes 2 3 1
      assertLeanParseError "List.length arg8 = 0"
        $ Where.LengthWhereSyntaxLimitExceeded
            Where.LengthWherePhysicalArgumentIndex 7 8 12
      case Where.parseLeanLengthWhereSource defaultLimits
          (BS.pack [0x30, 0x3d, 0x30, 0x20, 0x80]) of
        Left failure -> failure @?= Where.LengthWhereNonAsciiByte 4
        Right _ -> assertFailure "accepted non-ASCII Lean-shaped source"
  , testCase "produce the same scalar and pair contract sources" $ do
      compactScalar <- scalarContract defaultLimits observedOne
        "len(result)=len(arg0)+min(len(arg0),1)"
      leanScalar <- leanScalarContract observedOne
        "List.length result = List.length arg0 + min (List.length arg0) 1"
      leanScalar @?= compactScalar
      compactPair <- pairContract defaultLimits observedOne
        "len(result.first)+len(result.second)=2*len(arg0)"
      leanPair <- leanPairContract observedOne
        ("List.length result.1 + List.length result.2 " ++
          "= 2 * List.length arg0")
      leanPair @?= compactPair
  , testCase "re-export the Lean parser through the facade" $ do
      source <- case Djex.parseLeanLengthWhereSource Djex.defaultLengthLimits
          (ascii "List.length result = 0") of
        Left failure -> assertFailure (show failure) >> error "unreachable"
        Right value -> pure value
      Djex.elaborateLengthWhereSource Djex.LengthWhereScalar [] source @?=
        Right (Djex.LengthWhereScalarContractSource []
          $ scalarSource $ Length.LengthEqual result
              (Length.LengthLiteral 0))
  ]

relationTests :: TestTree
relationTests = testGroup "relations"
  [ relation "=" $ Length.LengthEqual result input0
  , relation "!=" $ Length.LengthNot $ Length.LengthEqual result input0
  , relation "<=" $ Length.LengthAtMost result input0
  , relation "<" $ Length.LengthNot $ Length.LengthAtMost input0 result
  , relation ">=" $ Length.LengthAtMost input0 result
  , relation ">" $ Length.LengthNot $ Length.LengthAtMost result input0
  ]
 where
  relation operator expected = testCase ("lower " ++ operator) $
    assertScalar defaultLimits observedOne
      ("len(result)" ++ operator ++ "len(arg0)") expected

arithmeticTests :: TestTree
arithmeticTests = testGroup "canonical arithmetic"
  [ testCase "respect product precedence and left associativity" $
      assertScalar defaultLimits observedThree
        "len(result)=len(arg0)+2*len(arg1)-len(arg2)/3%2"
        (Length.LengthEqual result
          (Length.LengthMonus
            (Length.LengthSum
              [ input0
              , Length.LengthScale 2 input1
              ])
            (Length.LengthModulo 2 $ Length.LengthQuotient 3 input2)))
  , testCase "fold and sort one canonical sum" $
      assertScalar defaultLimits observedTwo
        "len(result)=len(arg1)+1+len(arg0)+2"
        (Length.LengthEqual result $ Length.LengthSum
          [input0, input1, Length.LengthLiteral 3])
  , testCase "combine nested scales" $
      assertScalar defaultLimits observedOne
        "len(result)=2*(3*len(arg0))"
        (Length.LengthEqual result $ Length.LengthScale 6 input0)
  , testCase "fold natural subtraction and retain left-associated monus" $ do
      assertScalar defaultLimits observedOne "len(result)=3-8"
        (Length.LengthEqual result $ Length.LengthLiteral 0)
      assertScalar defaultLimits observedThree
        "len(result)=len(arg0)-len(arg1)-len(arg2)"
        (Length.LengthEqual result
          $ Length.LengthMonus
              (Length.LengthMonus input0 input1) input2)
  , testCase "flatten, deduplicate, sort, and fold minimum terms" $
      assertScalar defaultLimits observedTwo
        "len(result)=min(len(arg1),min(7,min(len(arg0),3)))"
        (Length.LengthEqual result
          $ Length.LengthMinimum
              (Length.LengthMinimum input0 input1)
              (Length.LengthLiteral 3))
  , testCase "flatten, deduplicate, sort, and fold maximum terms" $
      assertScalar defaultLimits observedTwo
        "len(result)=max(len(arg1),max(2,max(len(arg0),7)))"
        (Length.LengthEqual result
          $ Length.LengthMaximum
              (Length.LengthMaximum input0 input1)
              (Length.LengthLiteral 7))
  ]

divisorTests :: TestTree
divisorTests = testGroup "direct positive divisors"
  [ testCase "accept direct positive literals through parentheses" $ do
      assertScalar defaultLimits observedOne "len(result)=len(arg0)/(02)"
        (Length.LengthEqual result $ Length.LengthQuotient 2 input0)
      assertScalar defaultLimits observedOne "len(result)=len(arg0)%(3)"
        (Length.LengthEqual result $ Length.LengthModulo 3 input0)
      assertScalar defaultLimits [] "len(result)=9/2"
        (Length.LengthEqual result $ Length.LengthLiteral 4)
      assertScalar defaultLimits [] "len(result)=9%2"
        (Length.LengthEqual result $ Length.LengthLiteral 1)
  , testCase "reject folded, nonliteral, and zero divisors" $ do
      assertParseError "len(result)=len(arg0)/(1+1)"
        $ Where.LengthWhereNonliteralDivisor 21
      assertParseError "len(result)=len(arg0)/len(arg1)"
        $ Where.LengthWhereNonliteralDivisor 21
      assertParseError "len(result)=len(arg0)%0"
        $ Where.LengthWhereZeroDivisor 21
  ]

roleTests :: TestTree
roleTests = testGroup "physical roles and result namespaces"
  [ testCase "map physical arg1 and arg2 to compact input0 and input1" $
      assertScalar defaultLimits mixedRoles
        "len(result)=len(arg2)+len(arg1)"
        (Length.LengthEqual result $ Length.LengthSum [input0, input1])
  , testCase "retain observed but unmentioned physical roles" $ do
      source <- parseOK defaultLimits "len(result)=len(arg1)"
      Where.elaborateLengthWhereSource Where.LengthWhereScalar mixedRoles source
        @?= Right (Where.LengthWhereScalarContractSource mixedRoles
          $ scalarSource $ Length.LengthEqual result input0)
      assertSeals defaultLimits Where.LengthWhereScalar mixedRoles source
  , testCase "lower both pair results in the separate pair namespace" $
      assertPair defaultLimits mixedRoles
        "len(result.first)+len(arg2)=len(result.second)+len(arg1)"
        (Length.LengthEqual
          (Length.LengthSum [pairInput1, pairFirst])
          (Length.LengthSum [pairInput0, pairSecond]))
  , testCase "reject result namespaces selected by the other domain" $ do
      parsedPairSource <- parseOK defaultLimits "len(result.first)=0"
      Where.elaborateLengthWhereSource Where.LengthWhereScalar [] parsedPairSource
        @?= Left (Where.LengthWhereScalarDomainPairResult
          Length.LengthSpinePairFirst)
      scalarSourceValue <- parseOK defaultLimits "len(result)=0"
      Where.elaborateLengthWhereSource
        Where.LengthWhereBinaryProduct [] scalarSourceValue
        @?= Left Where.LengthWhereBinaryProductDomainScalarResult
  , testCase "reject unobserved, out-of-range, and overlong role vectors" $ do
      unobserved <- parseOK defaultLimits "len(arg0)=0"
      Where.elaborateLengthWhereSource Where.LengthWhereScalar mixedRoles
        unobserved @?= Left (Where.LengthWherePhysicalArgumentNotObserved 0)
      outOfRange <- parseOK defaultLimits "len(arg2)=0"
      Where.elaborateLengthWhereSource Where.LengthWhereScalar observedOne
        outOfRange @?= Left (Where.LengthWherePhysicalArgumentOutOfRange 2 1)
      noReferences <- parseOK defaultLimits "0=0"
      Where.elaborateLengthWhereSource Where.LengthWhereScalar
        (replicate 9 Length.LengthObservedSpine) noReferences @?=
          Left (Where.LengthWhereRoleVectorLimitExceeded 8 9)
  , testCase "retain folded references and report them in source order" $ do
      folded <- parseOK defaultLimits "0*len(arg2)+len(arg0)=0"
      Where.elaborateLengthWhereSource Where.LengthWhereScalar observedOne
        folded @?= Left (Where.LengthWherePhysicalArgumentOutOfRange 2 1)
      foldedUnobserved <- parseOK defaultLimits
        "0*len(arg0)+len(arg2)=0"
      Where.elaborateLengthWhereSource Where.LengthWhereScalar mixedRoles
        foldedUnobserved @?=
          Left (Where.LengthWherePhysicalArgumentNotObserved 0)
  , testCase "reject physical references when the configured input space is zero" $
      assertParseErrorUnder zeroInputLimits "len(arg0)=0"
        (Where.LengthWherePhysicalArgumentUnavailable 4)
  ]

byteAndOffsetTests :: TestTree
byteAndOffsetTests = testGroup "bytes and offsets"
  [ testCase "admit exactly 16384 bytes and reject the first excess" $ do
      let exact = ascii $ "0=0" ++ replicate 16381 ' '
          excess = BS.snoc exact 0x20
      source <- parseOKBytes defaultLimits exact
      assertSeals defaultLimits Where.LengthWhereScalar [] source
      assertParseErrorBytes excess
        (Where.LengthWhereSourceByteLimitExceeded 16384 16385)
  , testCase "reject the first non-ASCII byte at its exact byte offset" $
      assertParseErrorBytes
        (BS.pack [0x30, 0x3d, 0x30, 0x20, 0x80, 0xff])
        (Where.LengthWhereNonAsciiByte 4)
  , testCase "report exact EOF expectation offsets" $ do
      assertParseError "" $ Where.LengthWhereUnexpectedEnd 0
        Where.LengthWhereExpressionExpected
      assertParseError "1" $ Where.LengthWhereUnexpectedEnd 1
        Where.LengthWhereRelationExpected
      assertParseError "1=" $ Where.LengthWhereUnexpectedEnd 2
        Where.LengthWhereExpressionExpected
      assertParseError "len(" $ Where.LengthWhereUnexpectedEnd 4
        Where.LengthWhereReferenceExpected
      assertParseError "len(arg0" $ Where.LengthWhereUnexpectedEnd 8
        Where.LengthWhereRightParenthesisExpected
  , testCase "count spaces, tabs, CR, and LF as source bytes" $ do
      assertScalar defaultLimits [] "\r\n\t 1 = \n\r 1"
        (Length.LengthEqual (Length.LengthLiteral 1)
          (Length.LengthLiteral 1))
      assertParseError "\r\n\t1 == 1" $ Where.LengthWhereUnexpectedToken 5
        Where.LengthWhereRelationExpected
      assertParseError " \t0=0 \r\n?" $ Where.LengthWhereUnexpectedToken 8
        Where.LengthWhereEndExpected
  ]

resourceTests :: TestTree
resourceTests = testGroup "bounded canonical resources"
  [ testCase "admit nesting 64 and reject nesting 65 at the causal open" $ do
      let nested count = replicate count '(' ++ "0" ++ replicate count ')' ++ "=0"
      assertScalar defaultLimits [] (nested 64)
        (Length.LengthEqual (Length.LengthLiteral 0)
          (Length.LengthLiteral 0))
      assertParseError (nested 65) $ Where.LengthWhereSyntaxLimitExceeded
        Where.LengthWhereNestingDepth 64 65 64
  , testCase "count strict Not nodes and relation clauses in event order" $ do
      assertParseErrorUnder (limitsWithSyntax 2 32 64 256) "0!=0"
        $ Where.LengthWhereSyntaxLimitExceeded
            Where.LengthWhereSyntaxNodes 2 3 1
      assertParseErrorUnder (limitsWithSyntax 1024 1 64 256) "0=0"
        $ Where.LengthWhereSyntaxLimitExceeded
            Where.LengthWhereFormulaClauses 1 2 1
      assertParseErrorUnder (limitsWithSyntax 3 32 64 256)
        "len(arg0)<len(arg1)"
        $ Where.LengthWhereSyntaxLimitExceeded
            Where.LengthWhereSyntaxNodes 3 4 10
  , testCase "exclude scale factors and quotient/modulo divisors from nodes" $ do
      assertParseErrorUnder (limitsWithSyntax 4 32 64 256)
        "len(result)=2*len(arg0)"
        $ Where.LengthWhereSyntaxLimitExceeded
            Where.LengthWhereSyntaxNodes 4 5 14
      assertScalar (limitsWithSyntax 5 32 64 256) observedOne
        "len(result)=len(arg0)/2"
        (Length.LengthEqual result $ Length.LengthQuotient 2 input0)
      assertScalar (limitsWithSyntax 5 32 64 256) observedOne
        "len(result)=len(arg0)%2"
        (Length.LengthEqual result $ Length.LengthModulo 2 input0)
  , testCase "charge emitted sum width after folding" $ do
      assertScalar (limitsWithSyntax 1024 32 1 256) [] "1+2=3"
        (Length.LengthEqual (Length.LengthLiteral 3)
          (Length.LengthLiteral 3))
      assertParseErrorUnder (limitsWithSyntax 1024 32 1 256)
        "len(arg0)+len(arg1)=0"
        $ Where.LengthWhereSyntaxLimitExceeded
            Where.LengthWhereCollectionWidth 1 2 9
  , testCase "admit literal and physical-index maxima" $ do
      let maximumLiteral = show ((2 :: Integer) ^ (256 :: Int) - 1)
      assertScalar defaultLimits [] (maximumLiteral ++ "=0")
        (Length.LengthEqual
          (Length.LengthLiteral $ (2 :: Natural) ^ (256 :: Int) - 1)
          (Length.LengthLiteral 0))
      assertScalar defaultLimits (replicate 8 Length.LengthObservedSpine)
        "len(arg7)=0"
        (Length.LengthEqual
          (Length.LengthVariable $ Length.LengthInput 7)
          (Length.LengthLiteral 0))
  , testCase "bound huge numerals and the first unavailable physical index" $ do
      assertParseError (show ((2 :: Integer) ^ (256 :: Int)) ++ "=0")
        $ Where.LengthWhereSyntaxLimitExceeded
            Where.LengthWhereLiteralBits 256 257 0
      assertParseError (replicate 10000 '9' ++ "=0")
        $ Where.LengthWhereSyntaxLimitExceeded
            Where.LengthWhereLiteralBits 256 257 0
      assertParseError "len(arg8)=0" $ Where.LengthWhereSyntaxLimitExceeded
        Where.LengthWherePhysicalArgumentIndex 7 8 4
  , testCase "report derived arithmetic overflow at its causal operator" $ do
      let threeBits = limitsWithSyntax 1024 32 64 3
          twoBits = limitsWithSyntax 1024 32 64 2
      assertParseErrorUnder threeBits "4+4=0"
        $ Where.LengthWhereSyntaxLimitExceeded
            Where.LengthWhereLiteralBits 3 4 1
      assertParseErrorUnder twoBits "2*(2*len(arg0))=0"
        $ Where.LengthWhereSyntaxLimitExceeded
            Where.LengthWhereLiteralBits 2 3 1
      assertParseErrorUnder threeBits "min(3+4,6+2)=0"
        $ Where.LengthWhereSyntaxLimitExceeded
            Where.LengthWhereLiteralBits 3 4 9
  ]

refusalTests :: TestTree
refusalTests = testGroup "closed grammar refusals"
  [ testCase "reject nonlinear products" $
      assertParseError "len(arg0)*len(arg1)=0"
        $ Where.LengthWhereNonlinearProduct 9
  , testCase "reject chained comparisons before trailing-token handling" $
      assertParseError "0=0<=1" $ Where.LengthWhereChainedComparison 3
  , testCase "reject trailing source" $
      assertParseError "0=0 xyz" $ Where.LengthWhereUnexpectedToken 4
        Where.LengthWhereEndExpected
  , testCase "refuse the unsupported double-equals spelling" $
      assertParseError "0==0" $ Where.LengthWhereUnexpectedToken 1
        Where.LengthWhereRelationExpected
  ]

identityTests :: TestTree
identityTests = testGroup "semantic identity"
  [ testCase "ignore whitespace and leading zeros" $ do
      first <- scalarContract defaultLimits observedOne
        "len(result)=len(arg0)+2"
      second <- scalarContract defaultLimits observedOne
        " \r\n len( result ) = len( arg000 ) + 0002 \t"
      first @?= second
      firstFingerprint <- sealScalar defaultLimits observedOne first
      secondFingerprint <- sealScalar defaultLimits observedOne second
      firstFingerprint @?= secondFingerprint
  , testCase "ignore unrelated permissive Length limit values" $ do
      let permissive = expectLimits $ Length.defaultLengthLimitSource
            { Length.lengthLimitSourceTypeNodes = 8192
            , Length.lengthLimitSourceContractInputs = 16
            , Length.lengthLimitSourceSyntaxNodes = 2048
            , Length.lengthLimitSourceFormulaClauses = 64
            , Length.lengthLimitSourceCollectionWidth = 128
            , Length.lengthLimitSourceLiteralBits = 512
            , Length.lengthLimitSourceFingerprintBytes = 131072
            }
      first <- scalarContract defaultLimits observedOne
        "len(result)=len(arg0)+2"
      second <- scalarContract permissive observedOne
        "len(result)=len(arg0)+2"
      first @?= second
      sealScalar defaultLimits observedOne first >>=
        (sealScalar permissive observedOne second >>=) . (@?=)
  , testCase "match exact hand-built scalar and pair sources" $ do
      scalar <- scalarContract defaultLimits observedOne
        "len(result)=len(arg0)+1"
      scalar @?= scalarSource
        (Length.LengthEqual result
          $ Length.LengthSum [input0, Length.LengthLiteral 1])
      pair <- pairContract defaultLimits observedOne
        "len(result.first)=len(arg0)+1"
      pair @?= pairSource
        (Length.LengthEqual pairFirst
          $ Length.LengthSum [pairInput0, Length.LengthLiteral 1])
  , testCase "preserve checked contract, problem, query, and SMT bytes"
      assertCheckedPipelineParity
  , testCase
      "preserve checked pair contract, problem, query, and SMT bytes"
      assertCheckedPairPipelineParity
  ]

publicSurfaceTests :: TestTree
publicSurfaceTests = testGroup "public facade and opaque source"
  [ testCase "re-export the parser through Language.Haskell.Djex" $ do
      source <- case Djex.parseLengthWhereSource Djex.defaultLengthLimits
          (ascii "len(result)=0") of
        Left failure -> assertFailure (show failure) >> error "unreachable"
        Right value -> pure value
      Djex.elaborateLengthWhereSource Djex.LengthWhereScalar [] source @?=
        Right (Djex.LengthWhereScalarContractSource []
          $ scalarSource $ Length.LengthEqual result
              (Length.LengthLiteral 0))
  , testCase "hide source constructors, record fields, and instances" $
      lengthWhereSourceOpacityCharacterized `seq` pure ()
  , testCase "keep admitted source usable and errors sanitized" $ do
      source <- parseOK defaultLimits "len(result)=31337"
      assertSeals defaultLimits Where.LengthWhereScalar [] source
      let secret = "private-source-31337"
      rendered <- case Where.parseLengthWhereSource defaultLimits
          (ascii $ secret ++ "==0") of
        Left failure -> pure $ show failure
        Right _ -> assertFailure "sanitization probe unexpectedly parsed"
          >> error "unreachable"
      assertBool "a sanitized parse error retained source bytes"
        $ not (secret `isInfixOf` rendered)
  ]

defaultLimits :: Length.LengthLimits
defaultLimits = Length.defaultLengthLimits

observedOne, observedTwo, observedThree, mixedRoles
  :: [Length.LengthTargetArgumentRole]
observedOne = [Length.LengthObservedSpine]
observedTwo = replicate 2 Length.LengthObservedSpine
observedThree = replicate 3 Length.LengthObservedSpine
mixedRoles =
  [ Length.LengthUnobservedTarget
  , Length.LengthObservedSpine
  , Length.LengthObservedSpine
  ]

input0, input1, input2, result
  :: Length.LengthExpression Length.LengthContractVariable
input0 = Length.LengthVariable $ Length.LengthInput 0
input1 = Length.LengthVariable $ Length.LengthInput 1
input2 = Length.LengthVariable $ Length.LengthInput 2
result = Length.LengthVariable Length.LengthResult

pairInput0, pairInput1, pairFirst, pairSecond
  :: Length.LengthExpression Length.LengthSpinePairContractVariable
pairInput0 = Length.LengthVariable $ Length.LengthSpinePairInput 0
pairInput1 = Length.LengthVariable $ Length.LengthSpinePairInput 1
pairFirst = Length.LengthVariable
  $ Length.LengthSpinePairResult Length.LengthSpinePairFirst
pairSecond = Length.LengthVariable
  $ Length.LengthSpinePairResult Length.LengthSpinePairSecond

scalarSource
  :: Length.LengthFormula Length.LengthContractVariable
  -> Length.LengthContractSource
scalarSource postcondition = Length.LengthContractSource
  { Length.lengthContractPrecondition = Length.LengthTruth True
  , Length.lengthContractPostcondition = postcondition
  }

pairSource
  :: Length.LengthFormula Length.LengthSpinePairContractVariable
  -> Length.LengthSpinePairContractSource
pairSource postcondition = Length.LengthSpinePairContractSource
  { Length.lengthSpinePairContractPrecondition = Length.LengthTruth True
  , Length.lengthSpinePairContractPostcondition = postcondition
  }

ascii :: String -> BS.ByteString
ascii = BSC.pack

parseOK :: Length.LengthLimits -> String -> IO Where.LengthWhereSource
parseOK limits = parseOKBytes limits . ascii

parseOKBytes :: Length.LengthLimits -> BS.ByteString -> IO Where.LengthWhereSource
parseOKBytes limits source = case Where.parseLengthWhereSource limits source of
  Left failure -> assertFailure ("unexpected where rejection: " ++ show failure)
    >> error "unreachable"
  Right parsed -> pure parsed

parseHaskellOK :: String -> IO Where.LengthWhereSource
parseHaskellOK source =
  case Where.parseHaskellLengthWhereSource defaultLimits (ascii source) of
    Left failure -> assertFailure
      ("unexpected Haskell-shaped where rejection: " ++ show failure)
        >> error "unreachable"
    Right parsed -> pure parsed

assertHaskellParseError
  :: String -> Where.LengthWhereParseError -> Assertion
assertHaskellParseError = assertHaskellParseErrorUnder defaultLimits

assertHaskellParseErrorUnder
  :: Length.LengthLimits
  -> String
  -> Where.LengthWhereParseError
  -> Assertion
assertHaskellParseErrorUnder limits source expected =
  case Where.parseHaskellLengthWhereSource limits (ascii source) of
    Left actual -> actual @?= expected
    Right _ -> assertFailure "expected Haskell-shaped Length.Where rejection"

assertHaskellScalar
  :: [Length.LengthTargetArgumentRole]
  -> String
  -> Length.LengthFormula Length.LengthContractVariable
  -> Assertion
assertHaskellScalar roles source expected = do
  parsed <- parseHaskellOK source
  Where.elaborateLengthWhereSource Where.LengthWhereScalar roles parsed @?=
    Right (Where.LengthWhereScalarContractSource roles $ scalarSource expected)
  assertSeals defaultLimits Where.LengthWhereScalar roles parsed

assertHaskellPair
  :: [Length.LengthTargetArgumentRole]
  -> String
  -> Length.LengthFormula Length.LengthSpinePairContractVariable
  -> Assertion
assertHaskellPair roles source expected = do
  parsed <- parseHaskellOK source
  Where.elaborateLengthWhereSource Where.LengthWhereBinaryProduct roles parsed
    @?= Right (Where.LengthWhereBinaryProductContractSource roles
      $ pairSource expected)
  assertSeals defaultLimits Where.LengthWhereBinaryProduct roles parsed

haskellScalarContract
  :: [Length.LengthTargetArgumentRole]
  -> String
  -> IO Length.LengthContractSource
haskellScalarContract roles source = do
  parsed <- parseHaskellOK source
  case Where.elaborateLengthWhereSource Where.LengthWhereScalar roles parsed of
    Left failure -> assertFailure (show failure) >> error "unreachable"
    Right (Where.LengthWhereScalarContractSource returnedRoles contract) -> do
      returnedRoles @?= roles
      assertSeals defaultLimits Where.LengthWhereScalar roles parsed
      pure contract
    Right _ -> assertFailure "Haskell scalar elaboration returned pair source"
      >> error "unreachable"

haskellPairContract
  :: [Length.LengthTargetArgumentRole]
  -> String
  -> IO Length.LengthSpinePairContractSource
haskellPairContract roles source = do
  parsed <- parseHaskellOK source
  case Where.elaborateLengthWhereSource
      Where.LengthWhereBinaryProduct roles parsed of
    Left failure -> assertFailure (show failure) >> error "unreachable"
    Right (Where.LengthWhereBinaryProductContractSource returnedRoles
        contract) -> do
      returnedRoles @?= roles
      assertSeals defaultLimits Where.LengthWhereBinaryProduct roles parsed
      pure contract
    Right _ -> assertFailure "Haskell pair elaboration returned scalar source"
      >> error "unreachable"

parseLeanOK :: String -> IO Where.LengthWhereSource
parseLeanOK source =
  case Where.parseLeanLengthWhereSource defaultLimits (ascii source) of
    Left failure -> assertFailure
      ("unexpected Lean-shaped where rejection: " ++ show failure)
        >> error "unreachable"
    Right parsed -> pure parsed

assertLeanParseError
  :: String -> Where.LengthWhereParseError -> Assertion
assertLeanParseError = assertLeanParseErrorUnder defaultLimits

assertLeanParseErrorUnder
  :: Length.LengthLimits
  -> String
  -> Where.LengthWhereParseError
  -> Assertion
assertLeanParseErrorUnder limits source expected =
  case Where.parseLeanLengthWhereSource limits (ascii source) of
    Left actual -> actual @?= expected
    Right _ -> assertFailure "expected Lean-shaped Length.Where rejection"

assertLeanScalar
  :: [Length.LengthTargetArgumentRole]
  -> String
  -> Length.LengthFormula Length.LengthContractVariable
  -> Assertion
assertLeanScalar roles source expected = do
  parsed <- parseLeanOK source
  Where.elaborateLengthWhereSource Where.LengthWhereScalar roles parsed @?=
    Right (Where.LengthWhereScalarContractSource roles $ scalarSource expected)
  assertSeals defaultLimits Where.LengthWhereScalar roles parsed

assertLeanPair
  :: [Length.LengthTargetArgumentRole]
  -> String
  -> Length.LengthFormula Length.LengthSpinePairContractVariable
  -> Assertion
assertLeanPair roles source expected = do
  parsed <- parseLeanOK source
  Where.elaborateLengthWhereSource Where.LengthWhereBinaryProduct roles parsed
    @?= Right (Where.LengthWhereBinaryProductContractSource roles
      $ pairSource expected)
  assertSeals defaultLimits Where.LengthWhereBinaryProduct roles parsed

leanScalarContract
  :: [Length.LengthTargetArgumentRole]
  -> String
  -> IO Length.LengthContractSource
leanScalarContract roles source = do
  parsed <- parseLeanOK source
  case Where.elaborateLengthWhereSource Where.LengthWhereScalar roles parsed of
    Left failure -> assertFailure (show failure) >> error "unreachable"
    Right (Where.LengthWhereScalarContractSource returnedRoles contract) -> do
      returnedRoles @?= roles
      assertSeals defaultLimits Where.LengthWhereScalar roles parsed
      pure contract
    Right _ -> assertFailure "Lean scalar elaboration returned pair source"
      >> error "unreachable"

leanPairContract
  :: [Length.LengthTargetArgumentRole]
  -> String
  -> IO Length.LengthSpinePairContractSource
leanPairContract roles source = do
  parsed <- parseLeanOK source
  case Where.elaborateLengthWhereSource
      Where.LengthWhereBinaryProduct roles parsed of
    Left failure -> assertFailure (show failure) >> error "unreachable"
    Right (Where.LengthWhereBinaryProductContractSource returnedRoles
        contract) -> do
      returnedRoles @?= roles
      assertSeals defaultLimits Where.LengthWhereBinaryProduct roles parsed
      pure contract
    Right _ -> assertFailure "Lean pair elaboration returned scalar source"
      >> error "unreachable"

assertParseError :: String -> Where.LengthWhereParseError -> Assertion
assertParseError = assertParseErrorUnder defaultLimits

assertParseErrorBytes
  :: BS.ByteString -> Where.LengthWhereParseError -> Assertion
assertParseErrorBytes source expected =
  case Where.parseLengthWhereSource defaultLimits source of
    Left actual -> actual @?= expected
    Right _ -> assertFailure "expected Length.Where byte-source rejection"

assertParseErrorUnder
  :: Length.LengthLimits
  -> String
  -> Where.LengthWhereParseError
  -> Assertion
assertParseErrorUnder limits source expected =
  case Where.parseLengthWhereSource limits (ascii source) of
    Left actual -> actual @?= expected
    Right _ -> assertFailure "expected Length.Where parse rejection"

assertScalar
  :: Length.LengthLimits
  -> [Length.LengthTargetArgumentRole]
  -> String
  -> Length.LengthFormula Length.LengthContractVariable
  -> Assertion
assertScalar limits roles source expected = do
  parsed <- parseOK limits source
  Where.elaborateLengthWhereSource Where.LengthWhereScalar roles parsed @?=
    Right (Where.LengthWhereScalarContractSource roles $ scalarSource expected)
  assertSeals limits Where.LengthWhereScalar roles parsed

assertPair
  :: Length.LengthLimits
  -> [Length.LengthTargetArgumentRole]
  -> String
  -> Length.LengthFormula Length.LengthSpinePairContractVariable
  -> Assertion
assertPair limits roles source expected = do
  parsed <- parseOK limits source
  Where.elaborateLengthWhereSource Where.LengthWhereBinaryProduct roles parsed
    @?= Right (Where.LengthWhereBinaryProductContractSource roles
      $ pairSource expected)
  assertSeals limits Where.LengthWhereBinaryProduct roles parsed

scalarContract
  :: Length.LengthLimits
  -> [Length.LengthTargetArgumentRole]
  -> String
  -> IO Length.LengthContractSource
scalarContract limits roles source = do
  parsed <- parseOK limits source
  case Where.elaborateLengthWhereSource Where.LengthWhereScalar roles parsed of
    Left failure -> assertFailure (show failure) >> error "unreachable"
    Right (Where.LengthWhereScalarContractSource returnedRoles contract) -> do
      returnedRoles @?= roles
      assertSeals limits Where.LengthWhereScalar roles parsed
      pure contract
    Right _ -> assertFailure "scalar elaboration returned pair source"
      >> error "unreachable"

pairContract
  :: Length.LengthLimits
  -> [Length.LengthTargetArgumentRole]
  -> String
  -> IO Length.LengthSpinePairContractSource
pairContract limits roles source = do
  parsed <- parseOK limits source
  case Where.elaborateLengthWhereSource
      Where.LengthWhereBinaryProduct roles parsed of
    Left failure -> assertFailure (show failure) >> error "unreachable"
    Right (Where.LengthWhereBinaryProductContractSource returnedRoles contract) -> do
      returnedRoles @?= roles
      assertSeals limits Where.LengthWhereBinaryProduct roles parsed
      pure contract
    Right _ -> assertFailure "pair elaboration returned scalar source"
      >> error "unreachable"

assertSeals
  :: Length.LengthLimits
  -> Where.LengthWhereDomain
  -> [Length.LengthTargetArgumentRole]
  -> Where.LengthWhereSource
  -> Assertion
assertSeals limits domain roles parsed =
  case Where.elaborateLengthWhereSource domain roles parsed of
    Left failure -> assertFailure $ "representative did not elaborate: " ++
      show failure
    Right (Where.LengthWhereScalarContractSource returnedRoles source) ->
      case Length.sealRoleAwareLengthContract limits fixtureInventory
          returnedRoles (targetFor Where.LengthWhereScalar returnedRoles) source of
        Left failure -> assertFailure $ "elaboration did not seal: " ++ show failure
        Right _ -> pure ()
    Right (Where.LengthWhereBinaryProductContractSource returnedRoles source) ->
      case Length.sealRoleAwareLengthSpinePairContract limits fixtureInventory
          returnedRoles
          (targetFor Where.LengthWhereBinaryProduct returnedRoles) source of
        Left failure -> assertFailure $ "pair elaboration did not seal: " ++
          show failure
        Right _ -> pure ()

sealScalar
  :: Length.LengthLimits
  -> [Length.LengthTargetArgumentRole]
  -> Length.LengthContractSource
  -> IO [Word8]
sealScalar limits roles source = case Length.sealRoleAwareLengthContract limits
    fixtureInventory roles (targetFor Where.LengthWhereScalar roles) source of
  Left failure -> assertFailure (show failure) >> error "unreachable"
  Right checked -> pure $ Fingerprint.fingerprintCanonicalBytes
    $ Length.lengthContractFingerprint checked

targetFor
  :: Where.LengthWhereDomain
  -> [Length.LengthTargetArgumentRole]
  -> Type String
targetFor domain roles = foldr FunctionType resultType $ map argument roles
 where
  argument role = case role of
    Length.LengthObservedSpine -> spineType
    Length.LengthUnobservedTarget -> payloadType
  resultType = case domain of
    Where.LengthWhereScalar -> spineType
    Where.LengthWhereBinaryProduct -> TupleType Boxed [spineType, spineType]

payloadType :: Type String
payloadType = TupleType Boxed []

spineType :: Type String
spineType = TypeApplication (TypeConstructor listName) payloadType

fixtureInventory :: Inventory String ()
fixtureInventory = case mkInventory ClosedKindInventory
    ([] :: [Declaration String () ()]) of
  Left failure -> error $ "invalid Length.Where fixture inventory: " ++ show failure
  Right inventory -> inventory

zeroInputLimits :: Length.LengthLimits
zeroInputLimits = expectLimits $ Length.defaultLengthLimitSource
  { Length.lengthLimitSourceContractInputs = 0 }

limitsWithSyntax :: Int -> Int -> Int -> Int -> Length.LengthLimits
limitsWithSyntax nodes clauses width bits = expectLimits
  $ Length.defaultLengthLimitSource
      { Length.lengthLimitSourceSyntaxNodes = nodes
      , Length.lengthLimitSourceFormulaClauses = clauses
      , Length.lengthLimitSourceCollectionWidth = width
      , Length.lengthLimitSourceLiteralBits = bits
      }

expectLimits :: Length.LengthLimitSource -> Length.LengthLimits
expectLimits source = case Length.mkLengthLimits source of
  Left failure -> error $ "invalid Length.Where test limits: " ++ show failure
  Right limits -> limits

assertCheckedPipelineParity :: Assertion
assertCheckedPipelineParity = do
  parsedSource <- scalarContract defaultLimits observedOne
    " \r\nlen(result) = len(arg000) + 01\t"
  let handSource = scalarSource
        $ Length.LengthEqual fixtureResult
        $ Length.LengthSum [fixtureInput0, Length.LengthLiteral 1]
  parsedContract <- checkedFixtureContract parsedSource
  handContract <- checkedFixtureContract handSource
  Fingerprint.fingerprintCanonicalBytes
      (Length.lengthContractFingerprint parsedContract) @?=
    Fingerprint.fingerprintCanonicalBytes
      (Length.lengthContractFingerprint handContract)
  Length.checkedLengthContractPrecondition parsedContract @?=
    Length.checkedLengthContractPrecondition handContract
  Length.checkedLengthContractPostcondition parsedContract @?=
    Length.checkedLengthContractPostcondition handContract
  session <- checkedFixtureSession
  graph <- expectRightIO $ Djex.sealTermGraph Djex.sharedTypeStructure
    Djex.defaultTermGraphLimits fixtureIdentityGraphSource
  let candidate = fixtureTypedCandidate graph
  parsedProblem <- expectRightIO $ LengthProblem.sealLengthTypedCandidateProblem
    LengthProblem.defaultLengthProblemLimits session parsedContract
    candidate
  handProblem <- expectRightIO $ LengthProblem.sealLengthTypedCandidateProblem
    LengthProblem.defaultLengthProblemLimits session handContract
    candidate
  Fingerprint.fingerprintCanonicalBytes
      (LengthProblem.checkedLengthProblemEncodingFingerprint parsedProblem) @?=
    Fingerprint.fingerprintCanonicalBytes
      (LengthProblem.checkedLengthProblemEncodingFingerprint handProblem)
  LengthProblem.checkedLengthProblemPrecondition parsedProblem @?=
    LengthProblem.checkedLengthProblemPrecondition handProblem
  LengthProblem.checkedLengthProblemPostcondition parsedProblem @?=
    LengthProblem.checkedLengthProblemPostcondition handProblem
  parsedQuery <- expectRightIO $ SMTLib.sealLengthSMTLibQuery
    SMTLib.defaultLengthSMTLibLimits parsedProblem
  handQuery <- expectRightIO $ SMTLib.sealLengthSMTLibQuery
    SMTLib.defaultLengthSMTLibLimits handProblem
  Fingerprint.fingerprintCanonicalBytes
      (SMTLib.lengthSMTLibQueryFingerprint parsedQuery) @?=
    Fingerprint.fingerprintCanonicalBytes
      (SMTLib.lengthSMTLibQueryFingerprint handQuery)
  SMTLib.lengthSMTLibQueryCheckBytes parsedQuery @?=
    SMTLib.lengthSMTLibQueryCheckBytes handQuery
  SMTLib.lengthSMTLibQueryInputSymbols parsedQuery @?=
    SMTLib.lengthSMTLibQueryInputSymbols handQuery
  SMTLib.lengthSMTLibQueryInputValueRequestBytes parsedQuery @?=
    SMTLib.lengthSMTLibQueryInputValueRequestBytes handQuery

assertCheckedPairPipelineParity :: Assertion
assertCheckedPairPipelineParity = do
  parsedSource <- pairContract defaultLimits observedOne
    " \r\nlen(result.first) + len(arg000) = len(result.second) + 01\t"
  let handSource = pairSource
        $ Length.LengthEqual
            (Length.LengthSum [pairInput0, pairFirst])
            (Length.LengthSum [pairSecond, Length.LengthLiteral 1])
  parsedContract <- checkedFixturePairContract parsedSource
  handContract <- checkedFixturePairContract handSource
  Fingerprint.fingerprintCanonicalBytes
      (Length.lengthSpinePairContractFingerprint parsedContract) @?=
    Fingerprint.fingerprintCanonicalBytes
      (Length.lengthSpinePairContractFingerprint handContract)
  Length.checkedLengthSpinePairContractPrecondition parsedContract @?=
    Length.checkedLengthSpinePairContractPrecondition handContract
  Length.checkedLengthSpinePairContractPostcondition parsedContract @?=
    Length.checkedLengthSpinePairContractPostcondition handContract
  session <- checkedFixtureSession
  graph <- expectRightIO $ Djex.sealTermGraph Djex.sharedTypeStructure
    Djex.defaultTermGraphLimits fixtureInputAndZeroPairGraphSource
  let candidate = fixtureTypedCandidate graph
  parsedProblem <- expectRightIO
    $ LengthProblem.sealLengthSpinePairTypedCandidateProblem
        LengthProblem.defaultLengthProblemLimits session parsedContract candidate
  handProblem <- expectRightIO
    $ LengthProblem.sealLengthSpinePairTypedCandidateProblem
        LengthProblem.defaultLengthProblemLimits session handContract candidate
  Fingerprint.fingerprintCanonicalBytes
      (LengthProblem.checkedLengthSpinePairProblemEncodingFingerprint
        parsedProblem) @?=
    Fingerprint.fingerprintCanonicalBytes
      (LengthProblem.checkedLengthSpinePairProblemEncodingFingerprint
        handProblem)
  LengthProblem.checkedLengthSpinePairProblemPrecondition parsedProblem @?=
    LengthProblem.checkedLengthSpinePairProblemPrecondition handProblem
  LengthProblem.checkedLengthSpinePairProblemPostcondition parsedProblem @?=
    LengthProblem.checkedLengthSpinePairProblemPostcondition handProblem
  let parsedResult = LengthProblem.checkedLengthSpinePairCandidateResult
        $ LengthProblem.checkedLengthSpinePairProblemCandidate parsedProblem
  Length.lengthSpinePairFirst parsedResult @?= fixtureInput0
  Length.lengthSpinePairSecond parsedResult @?= Length.LengthLiteral 0
  parsedQuery <- expectRightIO $ SMTLib.sealLengthSpinePairSMTLibQuery
    SMTLib.defaultLengthSMTLibLimits parsedProblem
  handQuery <- expectRightIO $ SMTLib.sealLengthSpinePairSMTLibQuery
    SMTLib.defaultLengthSMTLibLimits handProblem
  Fingerprint.fingerprintCanonicalBytes
      (SMTLib.lengthSpinePairSMTLibQueryFingerprint parsedQuery) @?=
    Fingerprint.fingerprintCanonicalBytes
      (SMTLib.lengthSpinePairSMTLibQueryFingerprint handQuery)
  SMTLib.lengthSpinePairSMTLibQueryCheckBytes parsedQuery @?=
    SMTLib.lengthSpinePairSMTLibQueryCheckBytes handQuery
  SMTLib.lengthSpinePairSMTLibQueryInputSymbols parsedQuery @?=
    SMTLib.lengthSpinePairSMTLibQueryInputSymbols handQuery
  SMTLib.lengthSpinePairSMTLibQueryInputValueRequestBytes parsedQuery @?=
    SMTLib.lengthSpinePairSMTLibQueryInputValueRequestBytes handQuery

type FixtureType = Type (Variable String)

fixturePayload :: FixtureType
fixturePayload = TupleType Boxed []

fixtureSpine :: FixtureType
fixtureSpine = TypeApplication (TypeConstructor listName) fixturePayload

fixtureTarget :: FixtureType
fixtureTarget = FunctionType fixtureSpine fixtureSpine

fixturePairTarget :: FixtureType
fixturePairTarget = FunctionType fixtureSpine
  $ TupleType Boxed [fixtureSpine, fixtureSpine]

fixtureInput0, fixtureResult
  :: Length.LengthExpression Length.LengthContractVariable
fixtureInput0 = Length.LengthVariable $ Length.LengthInput 0
fixtureResult = Length.LengthVariable Length.LengthResult

fixtureSessionInventory :: Inventory (Variable String) ()
fixtureSessionInventory = case mkInventory ClosedKindInventory
    ([] :: [Declaration (Variable String) () ()]) of
  Left failure -> error $ "invalid pipeline fixture inventory: " ++ show failure
  Right inventory -> inventory

checkedFixtureSession
  :: IO (LengthProblem.CheckedLengthSession String ())
checkedFixtureSession = expectRightIO $ LengthProblem.sealLengthSession
  defaultLimits fixtureSessionInventory Length.BuiltinListSpine []

checkedFixtureContract
  :: Length.LengthContractSource
  -> IO (Length.CheckedLengthContract (Variable String))
checkedFixtureContract source = do
  session <- checkedFixtureSession
  expectRightIO $ Length.sealRoleAwareLengthContractInContext defaultLimits
    (LengthProblem.checkedLengthSessionContext session) observedOne fixtureTarget
    source

checkedFixturePairContract
  :: Length.LengthSpinePairContractSource
  -> IO (Length.CheckedLengthSpinePairContract (Variable String))
checkedFixturePairContract source = do
  session <- checkedFixtureSession
  expectRightIO
    $ Length.sealRoleAwareLengthSpinePairContractInContext defaultLimits
        (LengthProblem.checkedLengthSessionContext session) observedOne
        fixturePairTarget source

fixtureIdentityGraphSource :: Djex.TermGraphSource FixtureType Int
fixtureIdentityGraphSource = Djex.TermGraphSource (Djex.termNodeId 1)
  [ ( Djex.termNodeId 0
    , Djex.TermNode fixtureSpine
        $ Djex.TypedLocal (Djex.occurrenceId 1) 0
    )
  , ( Djex.termNodeId 1
    , Djex.TermNode fixtureTarget
        $ Djex.TypedLambda
            [ Djex.TypedPattern (Djex.occurrenceId 0) fixtureSpine
                $ Djex.TypedBind 0
            ]
            (Djex.termNodeId 0)
    )
  ]

fixtureInputAndZeroPairGraphSource :: Djex.TermGraphSource FixtureType Int
fixtureInputAndZeroPairGraphSource = Djex.TermGraphSource (Djex.termNodeId 3)
  [ ( Djex.termNodeId 0
    , Djex.TermNode fixtureSpine
        $ Djex.TypedLocal (Djex.occurrenceId 1) 0
    )
  , ( Djex.termNodeId 1
    , Djex.TermNode fixtureSpine
        $ Djex.TypedGlobal (Djex.occurrenceId 2) listName
    )
  , ( Djex.termNodeId 2
    , Djex.TermNode (TupleType Boxed [fixtureSpine, fixtureSpine])
        $ Djex.TypedTuple [Djex.termNodeId 0, Djex.termNodeId 1]
    )
  , ( Djex.termNodeId 3
    , Djex.TermNode fixturePairTarget
        $ Djex.TypedLambda
            [ Djex.TypedPattern (Djex.occurrenceId 0) fixtureSpine
                $ Djex.TypedBind 0
            ]
            (Djex.termNodeId 2)
    )
  ]

fixtureTypedCandidate
  :: Djex.TermGraph FixtureType Int
  -> Djex.TypedCandidate String FixtureType Int
      (Djex.Candidate FixtureType () ())
fixtureTypedCandidate graph = unsafeCoerce $ InternalTypedCandidate.mkTypedCandidate
  fixtureCompatibility (Right graph :: Either String (Djex.TermGraph FixtureType Int))

fixtureCompatibility :: Djex.Candidate FixtureType () ()
fixtureCompatibility = Djex.Candidate
  { Djex.candidateOutput = ()
  , Djex.candidateResidualConstraints = []
  , Djex.candidateDetails = ()
  }

expectRightIO :: Show failure => Either failure value -> IO value
expectRightIO outcome = case outcome of
  Left failure -> assertFailure ("unexpected fixture rejection: " ++ show failure)
    >> error "unreachable"
  Right value -> pure value
