module SourceKindRequestSpec (tests) where

import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import Language.Haskell.Djex.HaskellSrc
import qualified Language.Haskell.Djex.Djinn as D
import qualified Language.Haskell.Djex.Exference as E
import Language.Haskell.Djex.Exference.HaskellSrc (mkExferenceRequestWithCheckedTargetFromParsed)
import Language.Haskell.Exference.Core.Types (defaultVariableName)
import Language.Haskell.Synthesis.Declaration (Declaration (..))
import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Diagnostic (Diagnostic (diagnosticMessage))
import Language.Haskell.Synthesis.Environment (mkEnvironment)
import Language.Haskell.Synthesis.Generated (mkDefinitionName)
import Language.Haskell.Synthesis.Inventory (Inventory, mkInventory)
import Language.Haskell.Synthesis.Kind (Kind (..))
import Language.Haskell.Synthesis.KindInference (KindInventoryPolicy (ClosedKindInventory))
import Language.Haskell.Synthesis.Name (mkIdentifier, mkModuleName, parseName)
import Language.Haskell.Synthesis.Query
  ( QueryRequest (..), requestContextualType, requestContextualSourceKinds )
import Language.Haskell.Synthesis.SourceKind
import Language.Haskell.Synthesis.Type (Type (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests = testGroup "source kinds in parsed requests"
  [ testCase "public parser retains an explicit vacuous higher kind" $ do
      parsed <- parse "forall (f :: * -> *) a. a -> a"
      parsedSourceHasKindAnnotations parsed @?= True
      Map.lookup ([],0) (sourceBinderKinds $ parsedSourceKinds parsed) @?= Just higher
      assertBool "request obligations lost the annotation" $ at [] 0 higher `elem` parsedSourceRequestKinds parsed
  , testCase "implicit closure shifts a nested annotation into the new root" $ do
      parsed <- parse "a -> (forall (f :: * -> *) b. b -> b)"
      Map.lookup ([ForallBody,FunctionResult],0) (sourceBinderKinds $ parsedSourceKinds parsed) @?= Just higher
      assertBool "request annotation has stale pre-closure path" $
        at [ForallBody,FunctionResult] 0 higher `elem` parsedSourceRequestKinds parsed
  , testCase "tuple normalization updates source request kind positions" $ do
      parsed <- parse "((forall (f :: * -> *) a. a -> a), (forall (f :: *) a. a -> a))"
      Map.lookup ([TupleElement 0],0) (sourceBinderKinds $ parsedSourceKinds parsed) @?= Just higher
      Map.lookup ([TupleElement 1],0) (sourceBinderKinds $ parsedSourceKinds parsed) @?= Just proper
  , testCase "contradictory source annotation fails in the public parser" $ do
      inventory <- empty
      assertBool "contradictory kind entered a parsed request" $ isLeft $
        parseSourceType inventory "kinded-request" "forall (f :: *) a. f a -> f a"
  , testCase "explicit kinded forall still forbids an unbound type variable" $ do
      inventory <- empty
      assertBool "explicit source forall implicitly quantified a free variable" $ isLeft $
        parseSourceType inventory "kinded-request" "forall (f :: * -> *). a -> a"
  , testCase "ordinary source syntax keeps the unannotated request path" $ do
      parsed <- parse "forall f a. f a -> f a"
      parsedSourceHasKindAnnotations parsed @?= False
      parsedSourceRequestKinds parsed @?= []
      Map.lookup ([],0) (sourceBinderKinds $ parsedSourceKinds parsed) @?= Just higher
  , testCase "qualified kinded source honors an admitted import alias" $ do
      token <- right $ parseName "M.Token"
      canonical <- right $ mkModuleName "M"
      alias <- right $ mkModuleName "Q"
      inventory <- right $ mkInventory ClosedKindInventory
        ([AbstractTypeDeclaration () token proper] :: [Declaration String Void ()])
      let scope = ExferenceQueryScope Nothing [] [(alias,canonical)] [(alias,[token])]
      parsed <- right $ parseSourceTypeInScope inventory scope "kinded-request"
        "forall (f :: * -> *) a. a -> Q.Token"
      assertBool "alias resolution discarded kinds" $ parsedSourceHasKindAnnotations parsed
      case parsedSourceType parsed of
        ForallType _ _ (FunctionType _ (TypeConstructor name)) -> name @?= token
        other -> fail $ show other
  , testCase "kinded conversion cannot bypass a qualified import exclusion" $ do
      token <- right $ parseName "M.Token"
      canonical <- right $ mkModuleName "M"
      alias <- right $ mkModuleName "Q"
      inventory <- right $ mkInventory ClosedKindInventory
        ([AbstractTypeDeclaration () token proper] :: [Declaration String Void ()])
      let scope = ExferenceQueryScope Nothing [] [(alias,canonical)] [(alias,[])]
      assertBool "kinded source bypassed an empty qualified import" $ isLeft $
        parseSourceTypeInScope inventory scope "kinded-request" "forall (f :: * -> *) a. a -> Q.Token"
  , testCase "Djinn requests distinguish different kinds of the same vacuous binder" $ do
      parsed <- parse "forall (f :: * -> *) a. a -> a"
      query <- request (fmap defaultVariableName $ parsedSourceType parsed) D.defaultQueryOptions
      left <- right $ D.mkDjinnRequestWithSourceKinds [at [] 0 higher] query
      rightRequest <- right $ D.mkDjinnRequestWithSourceKinds [at [] 0 proper] query
      D.djinnRequestQuery left @?= D.djinnRequestQuery rightRequest
      assertBool "kind obligations were hidden in query-only equality" $ left /= rightRequest
      D.djinnRequestSourceKinds left @?= [at [] 0 higher]
  , testCase "Exference source factory retains the parser's exact kind obligations" $ do
      parsed <- parse "forall (f :: * -> *) a. a -> a"
      target <- right . mkDefinitionName =<< right (mkIdentifier "result")
      checked <- right $ mkExferenceRequestWithCheckedTargetFromParsed E.defaultExferenceOptions target parsed
      E.exferenceRequestSourceKinds checked @?= parsedSourceRequestKinds parsed
      query <- request (parsedSourceType parsed) E.defaultExferenceOptions
      alternate <- right $ E.mkExferenceRequestWithSourceKinds [at [] 0 proper,at [] 1 proper] query
      assertBool "Exference lost source kind identity" $ checked /= alternate
  , testCase "both request constructors reject an annotation owned by no binder" $ do
      parsed <- parse "forall a. a -> a"
      djinn <- request (fmap defaultVariableName $ parsedSourceType parsed) D.defaultQueryOptions
      exference <- request (parsedSourceType parsed) E.defaultExferenceOptions
      assertBool "Djinn accepted a stale source-kind path" $ isLeft $
        D.mkDjinnRequestWithSourceKinds [at [FunctionResult] 0 higher] djinn
      assertBool "Exference accepted a stale source-kind path" $ isLeft $
        E.mkExferenceRequestWithSourceKinds [at [FunctionResult] 0 higher] exference
  , testCase "empty kind metadata preserves existing request equality and display" $ do
      parsed <- parse "forall a. a -> a"
      djinn <- request (fmap defaultVariableName $ parsedSourceType parsed) D.defaultQueryOptions
      exference <- request (parsedSourceType parsed) E.defaultExferenceOptions
      dOld <- right $ D.mkDjinnRequest djinn
      dNew <- right $ D.mkDjinnRequestWithSourceKinds [] djinn
      eOld <- right $ E.mkExferenceRequest exference
      eNew <- right $ E.mkExferenceRequestWithSourceKinds [] exference
      dOld @?= dNew
      eOld @?= eNew
      show dOld @?= show dNew
      show eOld @?= show eNew
  , testCase "extra contexts move annotations below a newly inserted root" $ do
      cls <- right $ mkIdentifier "C"
      let scheme = ForallType ["f","a"] [] $ FunctionType (TypeVariable "a") (TypeVariable "a")
          goal = FunctionType scheme (TypeVariable "u")
      raw <- request goal ()
      let query = raw {requestContexts = [Constraint cls [TypeVariable "u"]]}
          moved = requestContextualSourceKinds query [at [FunctionParameter] 0 higher]
      moved @?= [at [ForallBody,FunctionParameter] 0 higher]
      validateSourceKindAnnotations (requestContextualType query) moved @?= Right ()
  , testCase "context insertion shifts only the last leading forall's existing arguments" $ do
      cls <- right $ mkIdentifier "C"
      let scheme = ForallType ["f","a"] [] $ FunctionType (TypeVariable "a") (TypeVariable "a")
          embedded = Constraint cls [scheme]
          goal = ForallType ["u"] [embedded] $ ForallType ["v"] [embedded] $
            FunctionType scheme (TypeVariable "u")
          annotations = [at [ForallConstraintArgument 0 0] 0 higher,
            at [ForallBody,ForallConstraintArgument 0 0] 0 proper,
            at [ForallBody,ForallBody,FunctionParameter] 0 higher]
      raw <- request goal ()
      let query = raw {requestContexts = [Constraint cls [TypeVariable "u"]]}
          moved = requestContextualSourceKinds query annotations
      moved @?= [at [ForallConstraintArgument 0 0] 0 higher,
        at [ForallBody,ForallConstraintArgument 1 0] 0 proper,
        at [ForallBody,ForallBody,FunctionParameter] 0 higher]
      validateSourceKindAnnotations (requestContextualType query) moved @?= Right ()
  , testCase "empty extra contexts preserve original annotation ownership" $ do
      parsed <- parse "forall (f :: * -> *) a. a -> a"
      query <- request (parsedSourceType parsed) ()
      requestContextualSourceKinds query (parsedSourceRequestKinds parsed) @?= parsedSourceRequestKinds parsed
  , testCase "both execution sessions reject a kind contradictory to its source uses" $ do
      parsed <- parse "forall f a. f a -> f a"
      djinnQuery <- request (fmap defaultVariableName $ parsedSourceType parsed) D.defaultQueryOptions
      exferenceQuery <- request (parsedSourceType parsed) E.defaultExferenceOptions
      djinn <- right $ D.mkDjinnRequestWithSourceKinds [at [] 0 proper] djinnQuery
      exference <- right $ E.mkExferenceRequestWithSourceKinds [at [] 0 proper] exferenceQuery
      dSession <- right D.standardDjinnSession
      environment <- right $ mkEnvironment []
      eSession <- right $ E.mkExferenceSession environment
      case D.runDjinnQuery dSession djinn of
        Left failure -> diagnosticMessage failure @?= "source binder kinds contradict the execution query"
        Right _ -> fail "Djinn discarded the contradictory kind"
      case E.runExferenceQuery eSession exference of
        Left failure -> diagnosticMessage failure @?= "source binder kinds contradict the execution query"
        Right _ -> fail "Exference discarded the contradictory kind"
  ]
 where
  proper = ProperTypeKind
  higher = FunctionKind proper proper
  at = SourceKindAnnotation
  parse source = do
    inventory <- empty
    right $ parseSourceType inventory "kinded-request" source

empty :: IO (Inventory String ())
empty = right $ mkInventory ClosedKindInventory ([] :: [Declaration String Void ()])

request :: ty -> options -> IO (QueryRequest ty options)
request goal options = do
  target <- right . mkDefinitionName =<< right (mkIdentifier "result")
  pure $ QueryRequest target goal [] options

right :: Show failure => Either failure value -> IO value
right = either (fail . show) pure
