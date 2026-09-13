module SourceKindParserSpec (tests) where

import qualified Data.Map.Strict as Map
import Data.Void (Void)
import Language.Haskell.Djex.HaskellSrc
  ( parseSourceType, parsedSourceType, parsedSourceKinds )
import Language.Haskell.Synthesis.Declaration (Declaration)
import Language.Haskell.Synthesis.Inventory (mkInventory, inventoryKindAssumptions)
import Language.Haskell.Synthesis.Kind (Kind (..))
import Language.Haskell.Synthesis.KindInference (KindInventoryPolicy (ClosedKindInventory))
import Language.Haskell.Synthesis.SourceKind
  ( SourceTypeStep (..), sourceKindsType, sourceBinderKinds, sourceKindAssumptions )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "parsed source binder kinds"
  [ testCase label $ do
      inventory <- right $ mkInventory ClosedKindInventory ([] :: [Declaration String Void ()])
      parsed <- right $ parseSourceType inventory "source-kinds" source
      let checked = parsedSourceKinds parsed
      sourceKindsType checked @?= parsedSourceType parsed
      sourceKindAssumptions checked @?= inventoryKindAssumptions inventory
      sourceBinderKinds checked @?= Map.fromList expected
  | (label, source, expected) <-
      [ ("explicit higher-kinded telescope", "forall f a. f a -> f a",
          [(([],0),higher),(([],1),ProperTypeKind)])
      , ("implicit source closure", "f a -> f a",
          [(([],0),higher),(([],1),ProperTypeKind)])
      , ("vacuous source binder retains proper default", "forall f a. a -> a",
          [(([],0),ProperTypeKind),(([],1),ProperTypeKind)])
      , ("nested scopes constrain the outer binder", "forall f. (forall a. f a -> f a)",
          [(([],0),higher),(([ForallBody],0),ProperTypeKind)])
      ]
  ]
 where
  higher = FunctionKind ProperTypeKind ProperTypeKind
  right result = either (fail . show) pure result
