-- Compile-time compatibility guard for Djinn's historical @HType(..)@
-- surface. The representation constructors are private; these bundled
-- patterns must continue to support ordinary constructor imports, complete
-- construction, and exhaustive matching from a downstream module.
module HTypeCompatibility
    ( hTypeCompatibilityTests
    ) where

import Test.Tasty.HUnit (Assertion, assertEqual)

import Djinn.Internal.HTypes
    ( HKind (KStar)
    , HType (HTAbstract, HTApp, HTArrow, HTCon, HTTuple, HTUnion, HTVar)
    )

hTypeCompatibilityTests :: [(String, Assertion)]
hTypeCompatibilityTests =
    [ ("construct and exhaustively match compatibility types", do
        let variable = HTVar "a"
            constructor = HTCon "Maybe"
            forms =
                [ HTApp constructor variable
                , variable
                , constructor
                , HTTuple [variable, constructor]
                , HTArrow variable constructor
                , HTUnion [("Just", [variable]), ("Nothing", [])]
                , HTAbstract "Opaque" KStar
                ]
        assertEqual "every bundled HType pattern remains available"
            [0 .. 6] $ map classify forms)
    ]

classify :: HType -> Int
classify source = case source of
    HTApp{} -> 0
    HTVar{} -> 1
    HTCon{} -> 2
    HTTuple{} -> 3
    HTArrow{} -> 4
    HTUnion{} -> 5
    HTAbstract{} -> 6
