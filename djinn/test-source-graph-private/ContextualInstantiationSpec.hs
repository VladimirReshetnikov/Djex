-- | Pure conditional-rule and dictionary-identity controls. These tests do
-- not claim search reachability, source-graph lowering, or GHC acceptance.
-- Structural fixtures supply explicit expected kinds; the production-gate
-- controls also exercise Core's actual source/root kind admission helper.
module ContextualInstantiationSpec (tests) where

import Control.Monad (forM_)
import Data.Either (rights)
import Data.List (isInfixOf)
import qualified Data.Set as Set
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Djinn.Internal.ContextualInstantiation as I
import Djinn.Internal.Environment
  ( PreparedEnvironment, prepareGroundSynthesisEnvironment
  , PreparedRootGivenOpening, prepareRootGivenOpening
  , rootGivenOpeningContexts, rootGivenOpeningBody
  , checkPreparedSynthesisTypesKinds, preparedEnvironmentSynthesisFormulaTranslator )
import Djinn.Internal.HTypes (HKind(KStar, KArrow))
import Djinn.Internal.LJTFormula
  ( Formula(..), Symbol(Symbol), Term(..), applys
  , dictionarySymbol, dictionarySymbolSource, opaqueTypeSymbol, opaqueSymbolSource
  , symbolSpelling )
import Djinn.Internal.ProofCheck.Evidence
  ( checkProofWithEvidence, checkedProofExpectedType )
import Language.Haskell.Synthesis.Constraint (Constraint(..))
import Language.Haskell.Synthesis.Declaration (Declaration(..), TypeParameter(..))
import Language.Haskell.Synthesis.Fresh (selectFresh)
import qualified Language.Haskell.Synthesis.Environment as E
import Language.Haskell.Synthesis.Kind (Kind(..))
import Language.Haskell.Synthesis.Name (Name, Boxity(Boxed), parseName)
import qualified Language.Haskell.Synthesis.Type as T
import qualified Language.Haskell.Synthesis.TypeAtom as A

type SourceType = T.Type String
type DeclarationSource = Declaration String Void ()

tests :: TestTree
tests = testGroup "conditional contextual instantiation (not synthesis)"
  [ dictionaryTests, substitutionTests, familyTests, kindAndRejectionTests
  , productionKindGateTests, conditionalProofTests ]

dictionaryTests :: TestTree
dictionaryTests = testGroup "dictionary atom identity"
  [ testCase "dictionary atoms cannot equal spellings or opaque qualified values" $ do
      let obligation = constraint "C" [atomA]
          dictionary = dictionarySymbol obligation
          ordinary = Symbol $ symbolSpelling dictionary
          qualifiedValue = opaqueTypeSymbol $ forallWith [] [obligation] unit
          symbols = [dictionary, ordinary, qualifiedValue]
      assertEqual "three logical namespaces collided" 3 $ Set.size $ Set.fromList symbols
      forM_ symbols $ \left -> forM_ symbols $ \rightSymbol ->
        assertEqual "equality and ordering disagree"
          (left == rightSymbol) (compare left rightSymbol == EQ)
      assertEqual "dictionary was exposed as an opaque value" Nothing $
        opaqueSymbolSource dictionary
      assertEqual "ordinary spelling became dictionary authority" Nothing $
        dictionarySymbolSource ordinary
      assertEqual "qualified value became dictionary authority" Nothing $
        dictionarySymbolSource qualifiedValue

  , testCase "alpha-equivalent bound arguments retain their exact source view" $ do
      let source = constraint "C" [identity "a"]
          renamed = constraint "C" [identity "b"]
      assertEqual "bound alpha-renaming changed dictionary identity"
        (dictionarySymbol source) (dictionarySymbol renamed)
      assertEqual "alpha identity replaced the original constraint tree"
        (Just source) $ dictionarySymbolSource $ dictionarySymbol source

  , testCase "free identities and nominal classes cannot donate dictionaries" $ do
      let sources =
            [ constraint "C" [var "a"]
            , constraint "C" [var "b"]
            , constraint "Other.C" [var "a"]
            , constraint "D" [var "a"]
            ]
      assertEqual "free or nominal identity was erased" (length sources) $
        Set.size $ Set.fromList $ map dictionarySymbol sources

  , testCase "constraint argument order and arity are structural identity" $ do
      let sources =
            [constraint "Rel" [atomA, atomB], constraint "Rel" [atomB, atomA]
            ,constraint "Rel" [atomA], constraint "Rel" [atomA, atomA]]
      assertEqual "dictionary arguments became an unordered pool" (length sources) $
        Set.size $ Set.fromList $ map dictionarySymbol sources
  ]

substitutionTests :: TestTree
substitutionTests = testGroup "source telescope and simultaneous substitution"
  [ testCase "one complete source owns its vector and ordered duplicate obligations" $ do
      prepared <- authority
      let source = forallWith ["a", "b"]
            [constraint "C" [var "b"], constraint "D" [var "a"], constraint "C" [var "b"]]
            $ tuple [var "a", var "b"]
          expectedContexts =
            [constraint "C" [atomB], constraint "D" [atomA], constraint "C" [atomB]]
          expectedBody = tuple [atomA, atomB]
      row <- single =<< right (request prepared source [atomA, atomB])
      assertEqual "original source scheme changed" source $ I.contextualInstantiationSource row
      assertEqual "correlated vector changed" [atomA, atomB] $ I.contextualInstantiationArguments row
      assertEqual "context order or duplicates changed" expectedContexts $
        I.contextualInstantiationObligations row
      assertEqual "body used another vector" expectedBody $ I.contextualInstantiationBody row
      assertEqual "dictionary arity" 3 $ I.contextualInstantiationDictionaryArity row
      assertEqual "provider plus dictionaries arity" 4 $ I.contextualInstantiationEvidenceArity row
      bodyFormula <- right $ compile prepared expectedBody
      assertEqual "conditional rule lost source ownership or an ordered premise"
        (PVar (opaqueTypeSymbol source) :-> foldr
          (\obligation result -> PVar (dictionarySymbol obligation) :-> result)
          bodyFormula expectedContexts) $ I.contextualInstantiationFormula row

  , testCase "shadowed leading binders receive distinct positional selections" $ do
      prepared <- authority
      let source = forallWith ["a"] [constraint "C" [var "a"]] $
            forallWith ["a"] [constraint "D" [var "a"]] $ arrow (var "a") token
      row <- single =<< right (request prepared source [atomA, atomB])
      assertEqual "shadowed source spelling was rewritten in its receipt" source $
        I.contextualInstantiationSource row
      assertEqual "an inner binder captured the outer context"
        [constraint "C" [atomA], constraint "D" [atomB]] $ I.contextualInstantiationObligations row
      assertEqual "the body selected the outer instead of inner binder"
        (arrow atomB token) $ I.contextualInstantiationBody row

  , testCase "simultaneous ranges do not rewrite a selected free variable" $ do
      prepared <- authority
      let source = forallWith ["a", "b"] [constraint "Rel" [var "a", var "b"]] $
            arrow (var "a") $ var "b"
      row <- single =<< right (request prepared source [var "b", atomA])
      assertEqual "the second assignment rewrote the first selected range"
        (arrow (var "b") atomA) $ I.contextualInstantiationBody row
      assertEqual "context substitution used different ranges"
        [constraint "Rel" [var "b", atomA]] $ I.contextualInstantiationObligations row

  , testCase "a nested binder cannot capture the selected free identity" $ do
      prepared <- authority
      let source = forallWith ["a"] [constraint "C" [var "a"]] $
            arrow (forallWith ["x"] [] $ arrow (var "a") $ var "x") $ var "a"
          expected = arrow
            (forallWith ["inner"] [] $ arrow (var "x") $ var "inner") $ var "x"
      row <- single =<< right (request prepared source [var "x"])
      assertBool "selected x was captured inside the callback" $
        A.alphaEquivalentTypes expected $ I.contextualInstantiationBody row
      assertEqual "the selected free identity disappeared" (Set.singleton "x") $
        T.freeVariables $ I.contextualInstantiationBody row
      assertEqual "source constraint captured a renamed callback binder"
        [constraint "C" [var "x"]] $ I.contextualInstantiationObligations row

  , testCase "a selected polytype keeps its own forall and qualification nested" $ do
      prepared <- authority
      let selected = forallWith ["b"] [constraint "D" [var "b"]] $
            arrow (var "b") $ var "b"
          source = forallWith ["a"] [constraint "C" [var "a"]] $ var "a"
      row <- single =<< right (request prepared source [selected])
      assertEqual "selected qualification became an original provider obligation"
        [constraint "C" [selected]] $ I.contextualInstantiationObligations row
      assertEqual "selected source telescope was consumed" selected $ I.contextualInstantiationBody row
      assertEqual "nested D contributed an extra dictionary argument" 1 $
        I.contextualInstantiationDictionaryArity row

  , testCase "a binderless qualification uses an empty type vector" $ do
      prepared <- authority
      let source = forallWith [] [constraint "C" [atomA]] token
      row <- single =<< right (request prepared source [])
      assertEqual "invented a type selection" [] $ I.contextualInstantiationArguments row
      assertEqual "lost the qualification" [constraint "C" [atomA]] $
        I.contextualInstantiationObligations row
      assertEqual "changed the body" token $ I.contextualInstantiationBody row
  ]

familyTests :: TestTree
familyTests = testGroup "one source-owned helper allocation"
  [ testCase "stable alpha dedup keeps the first exact source and each distinct vector" $ do
      prepared <- authority
      let source = provider "a"
          renamed = provider "renamed"
      firstRequest <- right $ request prepared source [atomA]
      duplicate <- right $ request prepared renamed [atomA]
      distinct <- right $ request prepared source [atomB]
      let family = I.contextualInstantiationAxioms [firstRequest, duplicate, distinct]
          rows = I.contextualInstantiations family
      assertEqual "alpha duplicate survived or a different vector was lost" 2 $ length rows
      assertEqual "first exact source view was replaced" [source, source] $
        map I.contextualInstantiationSource rows
      assertEqual "retained source order changed" [[atomA], [atomB]] $
        map I.contextualInstantiationArguments rows
      assertEqual "allocated helper identities collided" 2 $
        Set.size $ Set.fromList $ map I.contextualInstantiationSymbol rows
      assertEqual "premises detached from their sealed rows"
        [(I.contextualInstantiationSymbol row, I.contextualInstantiationFormula row) | row <- rows]
        $ I.contextualInstantiationPremises family

  , testCase "a rejected optional tuple does not discard another valid source row" $ do
      prepared <- authority
      let invalid = request prepared (provider "a") []
          attempts = [invalid,
            request prepared (provider "a") [atomA], request prepared (provider "a") [atomB]]
      rejectsWith "complete leading type vector" invalid
      let rows = I.contextualInstantiations $ I.contextualInstantiationAxioms $ rights attempts
      assertEqual "optional rejection consumed a valid row" [[atomA], [atomB]] $
        map I.contextualInstantiationArguments rows
      assertEqual "retained rows reused a separately allocated helper" 2 $
        Set.size $ Set.fromList $ map I.contextualInstantiationSymbol rows

  , testCase "the all-or-error convenience entrance preserves row failure" $ do
      prepared <- authority
      rejectsWith "complete leading type vector" $
        I.buildContextualInstantiationAxioms (checkType prepared) (compile prepared)
          [(provider "a", [atomA]), (provider "a", [])]
  ]

kindAndRejectionTests :: TestTree
kindAndRejectionTests = testGroup "explicit kind and admission boundaries"
  [ testCase "a context-only higher-kind binder is used and retains its declared kind" $ do
      prepared <- authority
      let source = forallWith ["f"] [constraint "HK" [var "f"]] token
      row <- single =<< right (requestWithKinds prepared [KArrow KStar KStar] source [unaryType])
      assertEqual "context-only higher-kind selection changed"
        [constraint "HK" [unaryType]] $ I.contextualInstantiationObligations row
      rejects "proper type discharged a unary class parameter" $
        I.prepareContextualInstantiation (checkType prepared) (compile prepared) source [atomA]

  , testCase "complete-type callbacks cannot replace original vector-kind authority" $ do
      prepared <- authority
      let source = forallWith ["f", "a"]
            [constraint "C" [T.TypeApplication (var "f") $ var "a"]] token
          substituted = forallWith []
            [constraint "C" [T.TypeApplication higherType unaryType]] token
      -- Both complete types have kind Type, yet the declared original vector
      -- is [Type -> Type, Type], not [(Type -> Type) -> Type, Type -> Type].
      -- Do not call the trusted sealer and require acceptance of this vector.
      right $ checkType prepared source
      right $ checkType prepared substituted
      rejects "changed correlated kinds passed the original vector obligations" $
        checkPreparedSynthesisTypesKinds prepared
          [(KArrow KStar KStar, higherType), (KStar, unaryType)]

  , testCase "a wholly vacuous binder requires independent vector-kind evidence" $ do
      prepared <- authority
      let source = forallWith ["unused"] [constraint "C" [atomA]] token
      rejectsWith "vacuous leading binder requires exact type-vector kind evidence" $
        I.prepareContextualInstantiation (checkType prepared) (compile prepared) source [unaryType]

  , testCase "one used binder does not hide a second vacuous source slot" $ do
      prepared <- authority
      let source = forallWith ["a", "unused"] [constraint "C" [var "a"]] $
            arrow (var "a") token
      rejectsWith "vacuous leading binder" $ request prepared source [atomA, atomB]

  , testCase "context-free source remains outside this conditional family" $ do
      prepared <- authority
      rejectsWith "requires a constrained source scheme" $
        request prepared (identity "a") [atomA]

  , testCase "wrong type-vector lengths are rejected instead of partially specialized" $ do
      prepared <- authority
      forM_ [[], [atomA, atomB]] $ \arguments ->
        rejectsWith "complete leading type vector" $ request prepared (provider "a") arguments

  , testCase "unknown classes and wrong declared constraint arities are rejected" $ do
      prepared <- authority
      forM_ [constraint "Unknown" [var "a"], constraint "C" [var "a", var "a"]] $ \obligation ->
        rejects "unvalidated class context acquired a conditional rule" $
          request prepared (forallWith ["a"] [obligation] $ arrow (var "a") token) [atomA]

  , testCase "the existing argument bound rejects a cyclic spine before its payloads" $
      rejectsWith "argument limit exceeded" $ I.prepareContextualInstantiation
        (error "argument-width rejection evaluated the type checker")
        (error "argument-width rejection evaluated the body compiler")
        (error "argument-width rejection evaluated the source")
        (repeat $ error "argument-width rejection evaluated a selected type")
  ]

productionKindGateTests :: TestTree
productionKindGateTests = testGroup "production source and root kind gate"
  [ testCase "the complete correlated vector retains original provider kinds" $ do
      prepared <- authority
      let source = forallWith ["f", "a"]
            [constraint "C" [T.TypeApplication (var "f") $ var "a"]] token
          good = [unaryType, atomA]
          bad = [higherType, unaryType]
          goodObligation = constraint "C" [T.TypeApplication unaryType atomA]
          badObligation = constraint "C" [T.TypeApplication higherType unaryType]
          root = forallWith [] [goodObligation, badObligation] $ arrow source token
      opening <- openRoot prepared root
      -- Both substituted qualifications are proper and both dictionaries are
      -- actual root Givens. Only the original correlated kinds exclude bad.
      right $ checkType prepared $ forallWith [] [badObligation] token
      right $ I.checkContextualInstantiationKinds prepared opening source good
      rejects "production gate changed the original correlated provider kinds" $
        I.checkContextualInstantiationKinds prepared opening source bad
      row <- single =<< right (I.prepareContextualInstantiation
        (checkType prepared) (compile prepared) source good)
      assertEqual "valid correlated source vector changed" good $
        I.contextualInstantiationArguments row
      assertBool "valid correlated obligation did not belong to the actual root"
        $ all (`elem` rootGivenOpeningContexts opening) $ I.contextualInstantiationObligations row
  , testCase "a selected vector cannot retune an original ambient defaulted kind" $ do
      prepared <- authority
      let applied = T.TypeApplication (var "g") $ var "b"
          source = forallWith ["g", "b"] [constraint "C" [var "b"]] $
            arrow (arrow applied applied) token
          root = forallWith ["f", "a"] [constraint "C" [unit]] $
            arrow source $ arrow (T.TypeApplication (var "f") $ var "a") token
      opening <- openRoot prepared root
      -- Read the actual opened identities from the opaque receipt's body;
      -- no spelling convention or independently renamed telescope is used.
      case rootGivenOpeningBody opening of
        T.FunctionType retainedSource (T.FunctionType (T.TypeApplication openedF openedA) result) -> do
          assertEqual "joint opening changed the final result" token result
          let good = [openedF, unit]
              bad = [openedA, unit]
          -- Without the original ambient obligations, a new inference pass
          -- can reinterpret a as unary and f as higher, satisfying these.
          right $ checkPreparedSynthesisTypesKinds prepared
            [(KStar, forallWith [] (rootGivenOpeningContexts opening) $
                rootGivenOpeningBody opening),
             (KArrow KStar KStar, openedA), (KStar, unit)]
          right $ I.checkContextualInstantiationKinds prepared opening retainedSource good
          rejects "production gate retuned the root's originally proper parameter" $
            I.checkContextualInstantiationKinds prepared opening retainedSource bad
          row <- single =<< right (I.prepareContextualInstantiation
            (checkType prepared) (compile prepared) retainedSource good)
          assertEqual "valid ambient selection changed" good $ I.contextualInstantiationArguments row
          assertEqual "valid ambient selection lost its root dictionary"
            (rootGivenOpeningContexts opening) $ I.contextualInstantiationObligations row
        other -> assertFailure $ "unexpected prepared ambient test body: " ++ show other
  ]

conditionalProofTests :: TestTree
conditionalProofTests = testGroup "independent propositional proof checks"
  [ testCase "a conditional helper consumes both the exact provider and dictionary" $ do
      prepared <- authority
      row <- single =<< right (request prepared (provider "a") [atomA])
      argumentFormula <- right $ compile prepared atomA
      resultFormula <- right $ compile prepared token
      let providerSymbol = Symbol "provider"
          dictionary = Symbol "given"
          argument = Symbol "argument"
          helper = I.contextualInstantiationSymbol row
          environment =
            [(helper, I.contextualInstantiationFormula row),
             (providerSymbol, PVar $ opaqueTypeSymbol $ provider "a"),
             (dictionary, PVar $ dictionarySymbol $ constraint "C" [atomA]),
             (argument, argumentFormula)]
          term = applys (Var helper) $ map Var [providerSymbol, dictionary, argument]
      evidence <- right $ checkProofWithEvidence environment resultFormula term
      assertEqual "checked conditional proof changed its expected result"
        resultFormula $ checkedProofExpectedType evidence

  , testCase "an opaque qualified unit cannot impersonate a dictionary argument" $ do
      prepared <- authority
      row <- single =<< right (request prepared (provider "a") [atomA])
      argumentFormula <- right $ compile prepared atomA
      resultFormula <- right $ compile prepared token
      let helper = I.contextualInstantiationSymbol row
          value = Symbol "qualifiedValue"
          supplied = Symbol "provider"
          argument = Symbol "argument"
          environment =
            [(helper, I.contextualInstantiationFormula row),
             (supplied, PVar $ opaqueTypeSymbol $ provider "a"),
             (value, PVar $ opaqueTypeSymbol $ forallWith [] [constraint "C" [atomA]] unit),
             (argument, argumentFormula)]
      rejects "qualified value supplied dictionary evidence" $
        checkProofWithEvidence environment resultFormula $
          applys (Var helper) $ map Var [supplied, value, argument]

  , testCase "a provider with its qualification erased cannot use the source-owned helper" $ do
      prepared <- authority
      row <- single =<< right (request prepared (provider "a") [atomA])
      resultFormula <- right $ compile prepared token
      argumentFormula <- right $ compile prepared atomA
      let helper = I.contextualInstantiationSymbol row
          supplied = Symbol "provider"
          dictionary = Symbol "given"
          argument = Symbol "argument"
          erased = forallWith ["a"] [] $ arrow (var "a") token
          environment =
            [(helper, I.contextualInstantiationFormula row),
             (supplied, PVar $ opaqueTypeSymbol erased),
             (dictionary, PVar $ dictionarySymbol $ constraint "C" [atomA]),
             (argument, argumentFormula)]
      rejects "an erased provider scheme borrowed the original source helper" $
        checkProofWithEvidence environment resultFormula $
          applys (Var helper) $ map Var [supplied, dictionary, argument]
  ]

name :: String -> Name
name = either (error . show) id . parseName

var :: String -> SourceType
var = T.TypeVariable

nominal :: String -> SourceType
nominal = T.TypeConstructor . name

atomA, atomB, token, unaryType, higherType, unit :: SourceType
atomA = nominal "A"
atomB = nominal "B"
token = nominal "Token"
unaryType = nominal "F"
higherType = nominal "Higher"
unit = tuple []

arrow :: SourceType -> SourceType -> SourceType
arrow = T.FunctionType

tuple :: [SourceType] -> SourceType
tuple = T.TupleType Boxed

forallWith :: [String] -> [Constraint SourceType] -> SourceType -> SourceType
forallWith = T.ForallType

constraint :: String -> [SourceType] -> Constraint SourceType
constraint spelling = Constraint $ name spelling

identity, provider :: String -> SourceType
identity binder = forallWith [binder] [] $ arrow (var binder) $ var binder
provider binder = forallWith [binder] [constraint "C" [var binder]] $ arrow (var binder) token

authority :: IO PreparedEnvironment
authority = do
  environment <- right $ E.mkEnvironment declarations
  right $ prepareGroundSynthesisEnvironment environment
 where
  unary = FunctionKind ProperTypeKind ProperTypeKind
  declarations :: [DeclarationSource]
  declarations =
    [ClassDeclaration () (name spelling)
      [TypeParameter parameter $ Just ProperTypeKind | parameter <- parameters] [] []
      | (spelling, parameters) <- [("C", ["a"]), ("D", ["a"]), ("Rel", ["a", "b"])]]
    ++ [ClassDeclaration () (name "HK") [TypeParameter "f" $ Just unary] [] []]
    ++ [AbstractTypeDeclaration () (name spelling) ProperTypeKind | spelling <- ["A", "B", "Token"]]
    ++ [AbstractTypeDeclaration () (name "F") unary,
        AbstractTypeDeclaration () (name "Higher") $ FunctionKind unary ProperTypeKind]

checkType :: PreparedEnvironment -> SourceType -> Either String ()
checkType prepared source = checkPreparedSynthesisTypesKinds prepared [(KStar, source)]

compile :: PreparedEnvironment -> SourceType -> Either String Formula
compile = preparedEnvironmentSynthesisFormulaTranslator

-- Mirror request preparation only to supply the actual search body to the
-- production opaque root-opening constructor; the kind tests consume that
-- checked receipt and the same gate called by Core.
openRoot :: PreparedEnvironment -> SourceType -> IO PreparedRootGivenOpening
openRoot prepared source = do
  (opened, _) <- right $ T.implicitizeLeadingForalls
    (const (Nothing :: Maybe ()))
    (\reserved variable -> Just $ selectFresh (++ "'") reserved (variable ++ "'"))
    Set.empty source
  let (_, _, body) = T.splitLeadingForalls opened
  result <- right $ prepareRootGivenOpening prepared source body
  case result of
    Just opening -> pure opening
    Nothing -> fail "kind-gate fixture did not establish a distinct root Given opening"

request
  :: PreparedEnvironment -> SourceType -> [SourceType]
  -> Either String I.ContextualInstantiationRequest
request prepared source arguments =
  requestWithKinds prepared (map (const KStar) arguments) source arguments

-- These expected kinds are fixture facts, not an inferred production policy.
-- They prevent the ordinary structural tests from supplying an unchecked
-- selected vector to a trusted module entrance.
requestWithKinds
  :: PreparedEnvironment -> [HKind] -> SourceType -> [SourceType]
  -> Either String I.ContextualInstantiationRequest
requestWithKinds prepared kinds source arguments = do
  if length kinds == length arguments then pure () else Left "fixture kind-vector arity mismatch"
  checkPreparedSynthesisTypesKinds prepared $ zip kinds arguments
  I.prepareContextualInstantiation (checkType prepared) (compile prepared) source arguments

single :: I.ContextualInstantiationRequest -> IO I.ContextualInstantiation
single request' = case I.contextualInstantiations $ I.contextualInstantiationAxioms [request'] of
  [row] -> pure row
  _ -> fail "a singleton sealed request did not allocate exactly one helper"

right :: Show failure => Either failure result -> IO result
right = either (fail . show) pure

rejects :: String -> Either String result -> IO ()
rejects label result = case result of
  Left _ -> pure ()
  Right _ -> assertFailure label

rejectsWith :: String -> Either String result -> IO ()
rejectsWith expected result = case result of
  Left failure -> assertBool ("unexpected rejection: " ++ failure) $ expected `isInfixOf` failure
  Right _ -> assertFailure $ "expected rejection containing: " ++ expected
