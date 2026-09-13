-- Compile this driver with the actual private Scope and HaskellSrcUtils
-- modules, then compile and execute the emitted fixtures independently.
-- Terms are rendering fixtures, never synthesis providers or acceptance.
module Main (main) where

import Control.Monad (forM_)
import Data.Either (isLeft)
import Language.Haskell.Djex.HaskellSrc.Scope
import System.Environment (getArgs)
import System.FilePath ((</>))

main :: IO ()
main = do
  arguments <- getArgs
  directory <- case arguments of
    [path] -> pure path
    _ -> fail "expected the fresh fixture output directory"
  declarations <- mapM render cases
  let common = unlines
        [ "{-# LANGUAGE RankNTypes, ImpredicativeTypes, TypeApplications, TypeAbstractions, ScopedTypeVariables, KindSignatures, NoPolyKinds, FlexibleContexts #-}"
        , "module Main where"
        ]
      positive = common ++ concat declarations ++ unlines
        [ "main :: IO ()"
        , "main = print (identityK @Maybe @Int (Just 17), vacuousK @Maybe @Int 23, nestedK @Int (\\x -> x) 31, implicitK @Bool True @Maybe @Int (Just 41), higherK @Maybe @[] @Int (Just [43]), layeredK @Maybe @Int 47, constrainedK @Int 53 @Maybe @Bool (Just True), shadowedK @Maybe @Int (Just 59) @[] @Bool [True])"
        ]
  writeFile (directory </> "Positive.hs") positive
  vacuous <- case filter (\(name, _, _) -> name == "vacuousK") cases of
    [fixture] -> render fixture
    _ -> fail "missing or duplicate vacuous rendering fixture"
  writeFile (directory </> "WrongKind.hs") $ common ++ vacuous ++ unlines
    [ "main :: IO ()", "main = print (vacuousK @Int @Int 23)" ]
  unlessCheck "unsupported polymorphic kind unexpectedly accepted" $
    isLeft $ standaloneSourceExpression "forall k (f :: k -> *). Int -> Int" "\\x -> x"
  unlessCheck "repeated leading binder unexpectedly accepted" $
    isLeft $ standaloneSourceExpression "forall a. forall a. a -> a" "\\x -> x"
  scopedTypeBinderNames "a -> (forall (f :: * -> *) b. f b -> f b)" `same` ["a"]
  scopedTypeBinderNames "Eq a => forall (f :: * -> *) b. f b -> f b" `same` ["a", "f", "b"]
  forM_ declarations $ \declaration -> unlessCheck "empty rendered declaration" $ not $ null declaration
  putStrLn "8 source rendering fixtures and 4 scope/rejection checks prepared"

render :: (String, String, String) -> IO String
render (name, signature, term) = do
  expression <- either fail pure $ standaloneSourceExpression signature term
  let closed = case scopedTypeBinderNames signature of
        [] -> signature
        _ -> case name of
          "implicitK" -> "forall a. " ++ signature
          "constrainedK" -> "forall a. " ++ signature
          _ -> signature
  pure $ name ++ " :: " ++ closed ++ "\n" ++ name ++ " = " ++ expression ++ "\n"

cases :: [(String, String, String)]
cases =
  [ ("identityK", "forall (f :: * -> *) a. f a -> f a", "\\x -> x")
  , ("vacuousK", "forall (f :: * -> *) a. a -> a", "\\x -> x")
  , ("nestedK", "forall a. (forall (f :: * -> *) b. f b -> f b) -> a -> a", "\\_ x -> x")
  , ("implicitK", "a -> (forall (f :: * -> *) b. f b -> f b)", "\\_ -> ((\\x -> x) :: forall (f :: * -> *) b. f b -> f b)")
  , ("higherK", "forall (f :: * -> *) (g :: * -> *) a. f (g a) -> f (g a)", "\\x -> x")
  , ("layeredK", "forall (f :: * -> *). forall a. a -> a", "\\x -> x")
  , ("constrainedK", "Eq a => a -> (forall (f :: * -> *) b. f b -> f b)", "\\_ -> ((\\x -> x) :: forall (f :: * -> *) b. f b -> f b)")
  , ("shadowedK", "forall (f :: * -> *) a. f a -> (forall (f :: * -> *) a. f a -> f a)", "\\_ -> ((\\x -> x) :: forall (f :: * -> *) a. f a -> f a)")
  ]

unlessCheck :: String -> Bool -> IO ()
unlessCheck message success = if success then pure () else fail message

same :: (Eq a, Show a) => a -> a -> IO ()
same actual expected = unlessCheck ("scope mismatch: " ++ show (actual, expected)) $ actual == expected
