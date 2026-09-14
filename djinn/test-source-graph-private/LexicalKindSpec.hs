module LexicalKindSpec (tests) where

import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import Djinn.Internal.Environment (prepareGroundSynthesisEnvironment, preparedEnvironmentInventory)
import Djinn.Internal.SourceGraph (checkSourceClauseGraph)
import Djinn.Internal.SourceTypingContext (sourceTypingContextWithCheckedGoal)
import Language.Haskell.Synthesis.Declaration (Declaration (..))
import qualified Language.Haskell.Synthesis.Environment as E
import qualified Language.Haskell.Synthesis.Generated as G
import Language.Haskell.Synthesis.Inventory (inventoryKindAssumptions)
import Language.Haskell.Synthesis.Kind (Kind (..))
import Language.Haskell.Synthesis.Name (Name, Boxity (Boxed), parseName)
import Language.Haskell.Synthesis.SourceKind
import qualified Language.Haskell.Synthesis.Type as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests = testGroup "checked lexical source kinds"
  [ testCase "vacuous higher-kinded root introduction retains its kind" $
      check True (forallT ["f","a"] $ arrow (var "a") (var "a"))
        [at [] 0 unary] (lam ["x"] $ G.Local "x")
  , testCase "local vacuous higher-kind selection accepts the matching constructor" $
      check True (arrow callback token) [at [FunctionParameter] 0 unary]
        (lam ["local"] $ select "local" unaryType)
  , testCase "local vacuous higher-kind selection rejects a proper type" $
      check False (arrow callback token) [at [FunctionParameter] 0 unary]
        (lam ["local"] $ select "local" token)
  , testCase "same-shaped adjacent callbacks retain distinct lexical kinds" $
      check True paired pairedKinds $ lam ["higher","proper"] $
        G.Tuple [select "higher" unaryType, select "proper" token]
  , testCase "adjacent callbacks cannot swap their kind authority" $
      check False paired pairedKinds $ lam ["higher","proper"] $
        G.Tuple [select "higher" token, select "proper" unaryType]
  , testCase "a shadowed local binder cannot borrow its outer namesake kind" $
      check False (forallT ["f"] $ arrow callback token)
        [at [] 0 unary, at [ForallBody,FunctionParameter] 0 ProperTypeKind]
        (lam ["local"] $ select "local" unaryType)
  , testCase "a shadowed local binder keeps its proper kind" $
      check True (forallT ["f"] $ arrow callback token)
        [at [] 0 unary, at [ForallBody,FunctionParameter] 0 ProperTypeKind]
        (lam ["local"] $ select "local" token)
  , testCase "non-vacuous higher-kinded identity survives opening and substitution" $
      check True (forallT ["f","a"] $ arrow (T.TypeApplication (var "f") (var "a"))
        (T.TypeApplication (var "f") (var "a"))) [at [] 0 unary]
        (lam ["x"] $ G.Local "x")
  ]
 where
  paired = arrow callback $ arrow callback $ T.TupleType Boxed [token,token]
  pairedKinds = [at [FunctionParameter] 0 unary,
    at [FunctionResult,FunctionParameter] 0 ProperTypeKind]

check :: Bool -> T.Type String -> [SourceKindAnnotation] -> G.Expression String -> IO ()
check expected goal annotations expression = do
  environment <- right $ E.mkEnvironment
    ([AbstractTypeDeclaration () (name "Token") ProperTypeKind,
      AbstractTypeDeclaration () (name "Unary") unary] :: [Declaration String Void ()])
  prepared <- right $ prepareGroundSynthesisEnvironment environment
  checked <- right $ prepareSourceTypeKinds
    (inventoryKindAssumptions $ preparedEnvironmentInventory prepared) goal annotations
  authority <- right $ sourceTypingContextWithCheckedGoal prepared checked Map.empty
  target <- right $ G.mkDefinitionName $ name "candidate"
  let result = checkSourceClauseGraph 813 authority $ G.functionClauseFromExpression target expression
  if expected then do
    _ <- right result
    pure ()
  else assertBool "wrong lexical kind was accepted" $ isLeft result

right :: Show e => Either e a -> IO a
right = either (fail . show) pure

name :: String -> Name
name = either (error . show) id . parseName

var :: String -> T.Type String
var = T.TypeVariable

token, unaryType, callback :: T.Type String
token = T.TypeConstructor $ name "Token"
unaryType = T.TypeConstructor $ name "Unary"
callback = forallT ["f"] token

forallT :: [String] -> T.Type String -> T.Type String
forallT variables = T.ForallType variables []

arrow :: T.Type String -> T.Type String -> T.Type String
arrow = T.FunctionType

unary :: Kind Void
unary = FunctionKind ProperTypeKind ProperTypeKind

at :: [SourceTypeStep] -> Int -> Kind Void -> SourceKindAnnotation
at = SourceKindAnnotation

lam :: [String] -> G.Expression String -> G.Expression String
lam names = G.Lambda $ map G.Bind names

select :: String -> T.Type String -> G.Expression String
select local ty = G.VisibleTypeApplication (G.Local local) $
  either (error . show) id $ G.specifiedVisibleTypeArgument ty
