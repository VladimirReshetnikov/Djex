{-# LANGUAGE PatternSynonyms #-}

-- | Acceptance specification for source-owned lexical class evidence.
-- The registered matrix covers the supported Given-only roles; targetTests
-- also includes Djinn's remaining constrained-provider search targets.
-- A rejected environment, missing graph, unsupported contextual
-- certificate, or missing witness is a failure, never a skipped candidate.
-- Passing this Given-only matrix would not cover instances, superclass paths,
-- class methods, or Lean's explicit dictionary projection.
module ContextEvidenceSpec (tests, targetTests) where

import Control.Exception (bracket, try)
import Control.Monad (forM, forM_)
import Data.List (intercalate, isInfixOf, nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Language.Haskell.Djex
import Language.Haskell.Djex.Exference.HaskellSrc (parseExferenceRequest)
import qualified Language.Haskell.Exference.Core.Expression as E
import qualified Language.Haskell.Exference.Core.ExpressionCheck as EC
import qualified Language.Haskell.Exference.Core.FunctionBinding as EF
import qualified Language.Haskell.Exference.Core.Types as ET
import qualified Language.Haskell.Synthesis.TypedGenerated as Q
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Text.Read (readMaybe)

data Engine = DjinnEngine | ExferenceEngine
  deriving (Eq, Show)

data Role
  = UnusedRootGiven
  | NestedQualifiedCallback
  | ExactForwarding
  | LocalGivenApplication
  | GlobalGivenApplication
  | SiblingLeakage
  deriving (Eq, Show)

data SearchOutput = SearchOutput
  { observedTerms :: [(String, Bool)]
    -- ^ Exact graph erasure and whether this candidate covers the named role.
  , searchDescription :: String
  }

data ProviderOrigin = LocalProvider | GlobalProvider Name
  deriving (Eq, Show)

data GraphObservation variable = GraphObservation
  { introducedGivens :: [(Q.EvidenceBinderId, Constraint (Type variable), Bool)]
    -- ^ The Boolean identifies a root telescope introduction, before any
    -- term lambda, application, tuple, or other term construction.
  , appliedGivens :: [(Q.EvidenceBinderId, ProviderOrigin)]
  }

tests :: TestTree
tests = contextualMatrix "Source-owned contextual evidence"
  [ (DjinnEngine, [UnusedRootGiven, NestedQualifiedCallback, ExactForwarding], False)
  , (ExferenceEngine, positiveRoles, True)
  ]

-- Full priority-3 Given acceptance, including the currently unsupported Djinn
-- loaded constrained environment and forced local specialization. Keep these
-- target assertions intact for the next search increment.
targetTests :: TestTree
targetTests = contextualMatrix "Complete contextual Given acceptance target"
  [(engine, positiveRoles, True) | engine <- [DjinnEngine, ExferenceEngine]]

contextualMatrix :: String -> [(Engine, [Role], Bool)] -> TestTree
contextualMatrix label rows = testGroup label
  ([ testGroup (engineName engine) $
       [ testCase (roleName role) $ bounded (roleName role) $
           executeSpecification engine role
       | role <- roles
       ] ++
       [ testCase "a qualified tuple component cannot supply its sibling" $
           bounded "sibling synthesis" $ do
             result <- synthesize engine SiblingLeakage
             assertEqual (searchDescription result) [] $ observedTerms result
       | checkSibling
       ]
   | (engine, roles, checkSibling) <- rows
   ] ++
   [ testCase "the public expression checker rejects sibling given leakage" $
       checkSiblingLeakage
   , testCase "GHC independently rejects sibling given leakage" $
       bounded "sibling GHC control" compileSiblingLeakage
   ])

positiveRoles :: [Role]
positiveRoles =
  [ UnusedRootGiven, NestedQualifiedCallback, ExactForwarding
  , LocalGivenApplication, GlobalGivenApplication
  ]

engineName :: Engine -> String
engineName DjinnEngine = "djinn"
engineName ExferenceEngine = "exference"

roleName :: Role -> String
roleName UnusedRootGiven = "unused_root_given"
roleName NestedQualifiedCallback = "nested_qualified_callback"
roleName ExactForwarding = "exact_contextual_forwarding"
roleName LocalGivenApplication = "forced_local_given_application"
roleName GlobalGivenApplication = "forced_global_given_application"
roleName SiblingLeakage = "sibling_given_leakage"

signature :: Role -> String
signature UnusedRootGiven = "forall a. C a => a -> a"
signature NestedQualifiedCallback =
  "((forall a. C a => a -> a) -> Token) -> Token"
signature ExactForwarding =
  "(forall a. C a => a -> Token) -> (forall b. C b => b -> Token)"
signature LocalGivenApplication =
  "forall a. C a => (forall b. C b => b -> Token) -> a -> Token"
signature GlobalGivenApplication = "forall a. C a => a -> Token"
signature SiblingLeakage =
  "forall a. ((C a => a -> Token), a -> Token)"

-- Search receives exactly the empty class C, abstract Token, and (only when
-- needed) token's generic constrained signature. It receives no instances,
-- constructors, class methods, observers, or implementations of target terms.
declarations :: Role -> IO [Declaration String kindVariable ()]
declarations role = do
  className <- expectRight $ parseName "C"
  tokenTypeName <- expectRight $ parseName "Token"
  providerName <- expectRight $ mkIdentifier "token"
  let variable = TypeVariable "a"
      providerType = ForallType ["a"] [Constraint className [variable]] $
        FunctionType variable $ TypeConstructor tokenTypeName
  pure $
    [ ClassDeclaration () className [TypeParameter "a" Nothing] [] []
    , AbstractTypeDeclaration () tokenTypeName ProperTypeKind
    ] ++
    [ ValueDeclaration $ ValueSignature () providerName providerType
    | role `elem` [GlobalGivenApplication, SiblingLeakage]
    ]

-- These finite bounds are acceptance budgets, not completeness claims. Inspect
-- every candidate in the observed window before behavioral selection: an
-- earlier unsupported graph must not be hidden by a later working candidate.
synthesize :: Engine -> Role -> IO SearchOutput
synthesize engine role = do
  sourceDeclarations <- declarations role
  target <- expectRight $ mkIdentifier $
    "context_" ++ engineName engine ++ "_" ++ roleName role
  providerName <- expectRight $ mkIdentifier "token"
  let expectedValues =
        [providerName | role `elem` [GlobalGivenApplication, SiblingLeakage]]
      checkInventory environment = do
        let actual = environmentDeclarations environment
        assertEqual "the source environment gained declarations"
          (length sourceDeclarations) $ length actual
        assertEqual "the source environment gained a value provider" expectedValues
          [valueName value | ValueDeclaration value <- actual]
        assertEqual "instances became search assumptions" (0 :: Int) $
          length [() | InstanceDeclaration{} <- actual]
        assertEqual "a constructor or method became a search provider" (0 :: Int) $
          length [() | DataTypeDeclaration{} <- actual]
            + sum [length methods | ClassDeclaration _ _ _ _ methods <- actual]
  case engine of
    DjinnEngine -> do
      environment <- expectRight (mkEnvironment sourceDeclarations :: Either
        (EnvironmentError DjinnTypeVariable) DjinnEnvironment)
      checkInventory environment
      session <- expectRight $ mkDjinnSession environment
      checkInventory $ djinnSessionEnvironment session
      request <- expectRight $ parseDjinnRequest session
        defaultQueryOptions
          { optionCutoff = 32, optionAlternatives = True, optionSorted = False
          , optionStrategy = Interleave, optionBudget = Just 20000
          }
        target "context-evidence-acceptance" $ signature role
      result <- expectRight $ runDjinnTypedQuery session request
      terms <- forM (batchCandidates $ resultSearch result) $ \candidate -> do
        let clause = candidateOutput $ typedCandidateCompatibility candidate
        graph <- either
          (\failure -> fail $ unlines [show engine, roleName role, show failure, show clause])
          pure $ typedCandidateTermGraph candidate
        assertEqual "Djinn graph changed its associated function clause" clause $
          eraseTermGraphToFunctionClause (clauseName clause) graph
        coversRole <- inspectGraph role expectedValues graph
        rendered <- expectRight $ renderExpression (defaultRenderOptions id) $
          eraseTermGraph graph
        pure (rendered, coversRole)
      pure $ SearchOutput terms $ show
        (engine, role, resultEvidence result, batchProgress $ resultSearch result)
    ExferenceEngine -> do
      let variable "a" = FlexibleVariable 0
          variable _ = FlexibleVariable 1
      environment <- expectRight (mkEnvironment
        (map (mapDeclarationTypeVariables variable) sourceDeclarations) :: Either
          (EnvironmentError ExferenceTypeVariable) ExferenceEnvironment)
      checkInventory environment
      session <- expectRight $ mkExferenceSession environment
      checkInventory $ exferenceSessionEnvironment session
      request <- expectRight $ parseExferenceRequest session
        defaultExferenceOptions
          { exferenceMaximumSteps = 20000, exferenceMaximumQueueSize = Just 256
          -- Check refutable obligations immediately; flexible obligations
          -- still defer under isPossible until their source types are fixed.
          , exferenceConstraintDeferralSteps = 0
          , exferenceAllowUnused = True, exferenceMultiConstructorPatterns = True
          }
        target "context-evidence-acceptance" $ signature role
      results <- expectRight $ runExferenceTypedQuery session request
      let candidates = take 32 $ concatMap (batchCandidates . resultSearch) results
      terms <- forM candidates $ \candidate -> do
        let clause = candidateOutput $ typedCandidateCompatibility candidate
        graph <- either
          (\failure -> fail $ unlines [show engine, roleName role, show failure, show clause])
          pure $ typedCandidateTermGraph candidate
        assertEqual "Exference graph changed its associated function clause" clause $
          eraseTermGraphToFunctionClause (clauseName clause) graph
        coversRole <- inspectGraph role expectedValues graph
        rendered <- expectRight $ renderExpression
          (defaultRenderOptions $ \local -> "v" ++ show local) $ eraseTermGraph graph
        pure (rendered, coversRole)
      pure $ SearchOutput terms $
        show (engine, role) ++ ": 32 candidates, 20000 steps, queue 256, constraint deferral 0"

inspectGraph
  :: (Ord variable, Show variable, Ord local)
  => Role -> [Name] -> Q.TermGraph (Type variable) local -> IO Bool
inspectGraph role allowedGlobals graph = do
  className <- expectRight $ parseName "C"
  tokenTypeName <- expectRight $ parseName "Token"
  providerName <- expectRight $ mkIdentifier "token"
  let globals = nub $ expressionGlobals $ Q.eraseTermGraph graph
  assertBool ("candidate acquired an undeclared provider: " ++ show globals) $
    all (`elem` allowedGlobals) globals
  observation <- visit className tokenTypeName Map.empty True $ Q.termGraphRoot graph
  let introductions = introducedGivens observation
      rootGivens = [binder | (binder, _, True) <- introductions]
      applications = appliedGivens observation
      requireOneRoot = assertEqual "the original root given lost its source slot"
        [0] $ map Q.evidenceBinderSlot rootGivens
      requireForced origin = do
        requireOneRoot
        assertBool "the constrained provider has no dictionary application" $
          not $ null applications
        forM_ applications $ \(binder, actualOrigin) -> do
          assertEqual "application selected a different provider origin" origin actualOrigin
          assertEqual "application did not use the exact root introduction/slot"
            rootGivens [binder]
  case role of
    UnusedRootGiven -> do
      requireOneRoot
      assertEqual "dictionary-independent identity acquired an application" [] applications
      pure True
    NestedQualifiedCallback -> do
      assertEqual "a nested callback given was promoted to the query root" [] rootGivens
      assertBool "the callback's unused qualified telescope was erased" $
        not $ null introductions
      assertEqual "dictionary-independent callback acquired an application" [] applications
      pure True
    ExactForwarding -> do
      let opaque = case Q.eraseTermGraph graph of
            Lambda [Bind variable] (Local returned) -> variable == returned
            _ -> False
          retainedScheme = any
            (\(_, Q.TermNode ty form) -> case form of
              Q.TypedLocal{} -> genericTokenProvider className tokenTypeName ty
              _ -> False)
            $ Q.termGraphNodes graph
      whenOpaque opaque $ assertBool "opaque forwarding erased its complete contextual scheme"
        retainedScheme
      pure opaque
    LocalGivenApplication -> requireForced LocalProvider >> pure True
    GlobalGivenApplication -> requireForced (GlobalProvider providerName) >> pure True
    SiblingLeakage -> fail "an uninhabited sibling query acquired a checked candidate"
 where
  node owner = maybe (fail $ "missing graph node: " ++ show owner) pure $
    Q.lookupTermNode owner graph

  visit className tokenTypeName active atRoot owner = do
    Q.TermNode _ form <- node owner
    case form of
      Q.TypedContextIntroduction occurrence child witness -> do
        let constraints = Q.contextIntroductionConstraints witness
            bindings =
              [ (Q.contextEvidenceBinder $ Q.givenContextEvidence occurrence slot, constraint)
              | (slot, constraint) <- zip [0 ..] constraints
              ]
        assertEqual "an introduction changed the declared C telescope" 1 $ length constraints
        forM_ constraints $ \constraint -> assertClass className constraint
        forM_ bindings $ \(binder, _) -> assertBool "an introduction reused an active evidence ID" $
          Map.notMember binder active
        rest <- visit className tokenTypeName (Map.union (Map.fromList bindings) active) atRoot child
        pure rest { introducedGivens =
          [(binder, constraint, atRoot) | (binder, constraint) <- bindings]
            ++ introducedGivens rest }
      Q.TypedContextApplication _ child witness -> do
        let constraints = Q.contextApplicationConstraints witness
            evidence = Q.contextApplicationEvidence witness
        assertEqual "context application lost its source-ordered arguments"
          (length constraints) $ length evidence
        assertEqual "an application changed the declared C telescope" 1 $ length constraints
        origin <- providerBase className tokenTypeName Set.empty child
        uses <- forM (zip constraints evidence) $ \(required, proof) -> do
          assertClass className required
          let binder = Q.contextEvidenceBinder proof
          actual <- maybe (fail "a dictionary reference escaped its lexical introduction") pure $
            Map.lookup binder active
          assertBool ("dictionary slot has the wrong class/type arguments: " ++ show (required, actual)) $
            sameConstraint required actual
          pure (binder, origin)
        rest <- visit className tokenTypeName active False child
        pure rest { appliedGivens = uses ++ appliedGivens rest }
      Q.TypedForallIntroduction _ child _ ->
        visit className tokenTypeName active atRoot child
      Q.TypedHole{} -> fail "a candidate graph retained an implementation hole"
      _ -> do
        children <- mapM (visit className tokenTypeName active False) $ references form
        pure $ GraphObservation
          (concatMap introducedGivens children) (concatMap appliedGivens children)

  providerBase className tokenTypeName visited owner = do
    assertBool "cyclic provider application spine" $ Set.notMember owner visited
    Q.TermNode ty form <- node owner
    let next = providerBase className tokenTypeName $ Set.insert owner visited
        retain origin = do
          assertBool "provider base lost its original forall and C constraint" $
            genericTokenProvider className tokenTypeName ty
          pure origin
    case form of
      Q.TypedLocal{} -> retain LocalProvider
      Q.TypedGlobal _ name -> retain $ GlobalProvider name
      Q.TypedVisibleTypeApplication _ child _ _ -> next child
      Q.TypedImplicitTypeApplication _ child _ -> next child
      Q.TypedContextApplication _ child _ -> next child
      _ -> fail "dictionary application has no retained local/global provider base"

  assertClass className (Constraint actual arguments) = do
    assertEqual "graph introduced an undeclared class" className actual
    assertEqual "graph changed C's parameter arity" 1 $ length arguments

  sameConstraint (Constraint left leftArguments) (Constraint right rightArguments) =
    left == right && length leftArguments == length rightArguments
      && and (zipWith (equivalentTypes sharedTypeStructure) leftArguments rightArguments)

  references form = case form of
    Q.TypedLocal{} -> []
    Q.TypedGlobal{} -> []
    Q.TypedLambda _ body -> [body]
    Q.TypedApply function argument _ -> [function, argument]
    Q.TypedVisibleTypeApplication _ child _ _ -> [child]
    Q.TypedForallIntroduction _ child _ -> [child]
    Q.TypedImplicitTypeApplication _ child _ -> [child]
    Q.TypedContextIntroduction _ child _ -> [child]
    Q.TypedContextApplication _ child _ -> [child]
    Q.TypedTuple children -> children
    Q.TypedHole{} -> []
    Q.TypedLet _ binding body -> [binding, body]
    Q.TypedCase scrutinee alternatives -> scrutinee : map snd alternatives

genericTokenProvider :: Eq variable => Name -> Name -> Type variable -> Bool
genericTokenProvider className tokenTypeName source = case source of
  ForallType [binder] [Constraint actualClass [TypeVariable constrained]]
      (FunctionType (TypeVariable parameter) (TypeConstructor result)) ->
    binder == constrained && binder == parameter
      && actualClass == className && result == tokenTypeName
  _ -> False

whenOpaque :: Bool -> IO () -> IO ()
whenOpaque True action = action
whenOpaque False _ = pure ()

-- Runtime definitions belong only to the independent replay. Empty C keeps
-- this increment about given identity, without smuggling in class methods.
-- Distinct callback payloads distinguish provider routing; no Token value is
-- available to search except through a supplied argument or token's scheme.
runtimePreamble :: [String]
runtimePreamble =
  [ "{-# LANGUAGE RankNTypes, ImpredicativeTypes, ScopedTypeVariables, TypeApplications, FlexibleContexts #-}"
  , "module Main where"
  , "class C a"
  , "instance C Int"
  , "instance C Bool"
  , "data Token = Token Int"
  , "observe :: Token -> Int"
  , "observe (Token payload) = payload"
  , "token :: forall a. C a => a -> Token"
  , "token _ = Token 211"
  , "suppliedA :: forall a. C a => a -> Token"
  , "suppliedA _ = Token 37"
  , "suppliedB :: forall a. C a => a -> Token"
  , "suppliedB _ = Token 91"
  , "inspectIdentity :: (forall a. C a => a -> a) -> Token"
  , "inspectIdentity f = Token (f (7 :: Int) + f (91 :: Int) + (if f True then 100 else 0) + (if f False then 1000 else 0))"
  , "inspectIdentityAgain :: (forall a. C a => a -> a) -> Token"
  , "inspectIdentityAgain f = Token (f (29 :: Int) + (if f False then 1 else 11))"
  ]

observations :: Role -> String -> String
observations role function = conjunction $ case role of
  UnusedRootGiven ->
    [ function ++ " (7 :: Int) == 7", function ++ " (91 :: Int) == 91"
    , function ++ " True", "not (" ++ function ++ " False)"
    ]
  NestedQualifiedCallback ->
    [ "observe (" ++ function ++ " inspectIdentity) == 198"
    , "observe (" ++ function ++ " inspectIdentityAgain) == 40"
    ]
  ExactForwarding -> supplied
  LocalGivenApplication -> supplied
  GlobalGivenApplication ->
    [ "observe (" ++ function ++ " (7 :: Int)) == 211"
    , "observe (" ++ function ++ " False) == 211"
    ]
  SiblingLeakage -> ["False"]
 where
  supplied =
    [ "observe (" ++ function ++ " suppliedA (7 :: Int)) == 37"
    , "observe (" ++ function ++ " suppliedB (91 :: Int)) == 91"
    , "observe (" ++ function ++ " suppliedA True) == 37"
    , "observe (" ++ function ++ " suppliedB False) == 91"
    ]

conjunction :: [String] -> String
conjunction = intercalate " && " . map (\value -> "(" ++ value ++ ")")

executeSpecification :: Engine -> Role -> IO ()
executeSpecification engine role = do
  result <- synthesize engine role
  let terms = observedTerms result
  assertBool ("no checked candidates: " ++ searchDescription result) $ not $ null terms
  assertBool ("no candidate covers the required graph role: " ++ searchDescription result) $
    any snd terms
  let named = zipWith (\index (term, qualifies) ->
        ("context_impl_" ++ show index, term, qualifies)) [0 :: Int ..] terms
      predicates = [observations role name | (name, _, _) <- named]
      fixture = unlines $ runtimePreamble ++ concat
        [ [name ++ " :: " ++ signature role, name ++ " = " ++ term]
        | (name, term, _) <- named
        ] ++
        [ "main :: IO ()"
        , "main = print ([" ++ intercalate ", " predicates ++ "], ["
            ++ intercalate ", " ["(" ++ p ++ ") && not (" ++ p ++ ")" | p <- predicates]
            ++ "])"
        ]
  replay <- executeModule fixture
  case replay of
    (ExitSuccess, output, errors) -> case readMaybe output :: Maybe ([Bool], [Bool]) of
      Just (passes, falseControls) -> do
        assertEqual "GHC skipped a candidate's behavioral observations" (length terms) $ length passes
        assertEqual "GHC skipped a contradictory control" (length terms) $ length falseControls
        assertBool (unlines ["no matching implementation", searchDescription result, errors, fixture]) $
          or $ zipWith (&&) (map snd terms) passes
        assertBool "a contradictory predicate accepted a candidate" $ not $ or falseControls
      Nothing -> fail $ "malformed GHC execution result: " ++ output ++ errors
    _ -> fail $ "GHC rejected the exact candidate at its complete signature: "
      ++ show replay ++ "\n" ++ fixture

-- The positive checker control shares its environment and expression with the
-- first component of the negative control. Rejection must concern constraints,
-- not an unknown class, malformed provider, or an unrelated expression error.
checkSiblingLeakage :: IO ()
checkSiblingLeakage = do
  className <- expectRight $ parseName "C"
  tokenTypeName <- expectRight $ parseName "Token"
  providerName <- expectRight $ mkIdentifier "token"
  classes <- expectRight $ ET.mkStaticClassEnv [ET.HsTypeClass className [0] []] []
  let given variable = ET.HsConstraint className [ET.TypeVar variable]
      result = ET.TypeCons tokenTypeName
      arrow = ET.TypeArrow (ET.TypeVar 0) result
      provider = EF.FunctionBinding result providerName 0 [given 5] [ET.TypeVar 5]
      environment = ET.mkQueryClassEnv classes []
      body variable = E.ExpLambda variable (ET.TypeVar 0) $
        E.ExpApply (E.ExpName providerName) $ E.ExpVar variable $ ET.TypeVar 0
      positive = ET.TypeForall [0] [given 0] arrow
      negative = ET.TypeForall [0] [] $
        ET.TypeTuple Boxed [ET.TypeForall [] [given 0] arrow, arrow]
  assertEqual "the matched lexical-given control failed" (Right ()) $
    EC.checkExpression environment [provider] [] positive [] $ body 1
  case EC.checkExpression environment [provider] [] negative [] $
      E.ExpTuple [body 1, body 2] of
    Left (EC.ConstraintMismatch _ unresolved) ->
      assertBool "the sibling rejection lost its unsatisfied obligation" $ not $ null unresolved
    Left (EC.RefutableConstraints unresolved) ->
      assertBool "the sibling rejection lost its refutable obligation" $ not $ null unresolved
    other -> fail $ "expected a scoped constraint rejection, got " ++ show other

compileSiblingLeakage :: IO ()
compileSiblingLeakage = do
  let positive = unlines $ runtimePreamble ++
        [ "positive :: forall a. C a => a -> Token"
        , "positive x = token x"
        , "main :: IO ()"
        , "main = print (observe (positive (7 :: Int)) == 211)"
        ]
      negative = unlines $ runtimePreamble ++
        [ "leak :: " ++ signature SiblingLeakage
        , "leak = ((\\x -> token x), (\\x -> token x))"
        , "main :: IO ()"
        , "main = print True"
        ]
  positiveResult <- executeModule positive
  case positiveResult of
    (ExitSuccess, output, _) -> assertEqual "GHC skipped the positive control"
      (Just True) (readMaybe output :: Maybe Bool)
    _ -> fail $ "GHC positive contextual control failed: " ++ show positiveResult
  negativeResult <- executeModule negative
  case negativeResult of
    (ExitFailure _, _, errors) -> do
      assertBool ("GHC rejection did not identify the missing C dictionary: " ++ errors) $
        "C a" `isInfixOf` errors
      assertBool ("GHC rejection did not identify the constrained provider: " ++ errors) $
        "token" `isInfixOf` errors
    _ -> fail $ "GHC accepted sibling dictionary leakage: " ++ show negativeResult

executeModule :: String -> IO (ExitCode, String, String)
executeModule source = withTemporaryModule source $ \path -> do
  result <- timeout 60000000 $ readProcessWithExitCode "runghc" [path] ""
  maybe (fail $ "independent GHC replay timed out\n" ++ source) pure result

bounded :: String -> IO value -> IO value
bounded label action = do
  result <- timeout 90000000 action
  maybe (fail $ label ++ " exceeded its 90-second acceptance bound") pure result

expectRight :: Show error => Either error value -> IO value
expectRight = either (fail . show) pure

withTemporaryModule :: String -> (FilePath -> IO value) -> IO value
withTemporaryModule source action = do
  temporaryDirectory <- getTemporaryDirectory
  bracket (openTempFile temporaryDirectory "djex-context-evidence.hs") cleanup $
    \(path, handle) -> do
      hPutStr handle source
      hClose handle
      action path
 where
  cleanup (path, handle) = do
    _ <- try (hClose handle) :: IO (Either IOError ())
    removeFile path
