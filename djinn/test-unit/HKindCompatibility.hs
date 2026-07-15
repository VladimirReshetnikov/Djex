{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Source-compatibility checks for Djinn's historical kind vocabulary.
--
-- This module deliberately imports @HKind(..)@ rather than naming the three
-- patterns separately.  It therefore checks that the compatibility patterns
-- remain bundled with the native type, just as the former data constructors
-- were.  The exhaustive definition of 'kindView' additionally requires the
-- patterns' @COMPLETE@ declaration when 'HKind' is represented by the shared
-- kind tree.
module HKindCompatibility
  ( hKindCompatibilityTests
  ) where

import Djinn.Core
  ( Declaration (AbstractType)
  , declare
  , emptyEnvironment
  )
import Djinn.Internal.HCheck (htCheckTypeKind)
import Djinn.Internal.HTypes (HKind (..), HType (HTVar))
import Test.Tasty.HUnit (Assertion, (@?=))

data KindView
  = ProperTypeView
  | FunctionView KindView KindView
  | VariableView Int
  deriving (Eq, Show)

-- Keep this definition warning-clean under @-Wincomplete-patterns@.  For the
-- native representation, that is a compile-time check of the compatibility
-- patterns' COMPLETE set rather than merely a runtime case analysis.
kindView :: HKind -> KindView
kindView KStar = ProperTypeView
kindView (KArrow parameter result) =
  FunctionView (kindView parameter) (kindView result)
kindView (KVar variable) = VariableView variable

hKindCompatibilityTests :: [(String, Assertion)]
hKindCompatibilityTests =
  [ ("construct and exhaustively match compatibility kinds", do
      let kind = KArrow (KVar 3) (KArrow KStar (KVar 4))
      kindView kind @?=
        FunctionView
          (VariableView 3)
          (FunctionView ProperTypeView (VariableView 4))
    )
  , ("render compatibility kinds exactly", do
      show KStar @?= "*"
      show (KVar 7) @?= "k7"
      show (KArrow KStar (KArrow KStar KStar)) @?= "* -> * -> *"
      show (KArrow (KArrow KStar KStar) KStar) @?= "(* -> *) -> *"
      showsPrec 1 (KArrow KStar KStar) "" @?= "(* -> *)"
    )
  , ("retain the Core unsolved-kind diagnostic", do
      declare (AbstractType "Mystery" $ KVar 12) emptyEnvironment @?=
        Left "kind contains an unsolved variable: k12"
    )
  , ("retain the raw HCheck unsolved-kind diagnostic", do
      htCheckTypeKind [] (KVar 12) (HTVar "a") @?=
        Left "kind contains an unsolved variable: 12"
    )
  ]
