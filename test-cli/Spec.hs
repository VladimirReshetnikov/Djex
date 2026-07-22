module Main (main) where

import CLIAssertions (assertContains, countOccurrences)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Char (toLower)
import Data.List (intercalate, isInfixOf)
import System.Directory
  ( canonicalizePath
  , createDirectory
  , createDirectoryLink
  , createDirectoryIfMissing
  , createFileLink
  , getTemporaryDirectory
  , removeFile
  , removePathForcibly
  )
import System.Exit (ExitCode (..))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit
  ( Assertion
  , assertBool
  , assertEqual
  , testCase
  )
import qualified Language.Haskell.Djex as Djex

main :: IO ()
main = defaultMain $ testGroup "Djex CLI integration"
  [ testCase "global help and version load no backend" testGlobalInformation
  , testCase "REPL dispatch validates startup options without loading"
      testReplDispatch
  , testCase "REPL keeps both backend sessions and settings alive"
      testReplSharedSession
  , testCase "REPL multiline, repeat, and command errors recover"
      testReplInputRecovery
  , testCase "REPL both mode isolates an unavailable backend"
      testReplBackendIsolation
  , testCase "REPL environment replacement is transactional"
      testReplTransactionalLoad
  , testCase "REPL source targets load canonically and dependency-first"
      testReplWorkspaceTargets
  , testCase "REPL default environment keeps its full automatic context"
      testReplDefaultEnvironmentScope
  , testCase "REPL reload refreshes derived module target spellings"
      testReplReloadTargetSpelling
  , testCase "REPL named symlink aliases retain every expectation"
      testReplNamedSymlinkTargets
  , testCase "REPL directory loading distinguishes file and directory links"
      testReplDirectoryLinks
  , testCase "REPL target additions and removals are atomic"
      testReplTargetMutation
  , testCase "REPL reload and target mutations rebuild GHCi contexts"
      testReplScopeRetention
  , testCase "REPL module contexts follow GHCi replacement semantics"
      testReplModuleContexts
  , testCase "REPL imports and browsing honor explicit exports"
      testReplImportsAndBrowsing
  , testCase "REPL :type parses expressions without replacing query history"
      testReplTypeInference
  , testCase "REPL :type resolves only terms in the current module scope"
      testReplTypeScope
  , testCase "REPL :type +d defaults eligible numeric result variables"
      testReplTypeDefaulting
  , testCase "REPL Exference search is restricted to the prompt scope"
      testReplSearchScope
  , testCase "REPL aliases retain canonical re-exports and defer ambiguity"
      testReplAliasesAndReexports
  , testCase "REPL unresolved import lists remain advisory"
      testReplUnresolvedImportList
  , testCase "REPL import lists preserve exported record selectors"
      testReplRecordSelectors
  , testCase "REPL bundled imports respect abstract record exports"
      testReplAbstractRecordExports
  , testCase "REPL source modules reject package-qualified local lookalikes"
      testReplSourcePackageImport
  , testCase "REPL symlink aliases cannot impersonate module names"
      testReplSymlinkModuleMismatch
  , testCase "REPL re-exports reject only same-namespace collisions"
      testReplExportAmbiguity
  , testCase "REPL bundled imports reject type-synonym wildcards"
      testReplBundledOwners
  , testCase "REPL import failures roll back without touching Djinn"
      testReplImportRollback
  , testCase "REPL fix policy rebuilds the Exference session"
      testReplFixReload
  , testCase "REPL scripts persist state and reject recursion"
      testReplScripts
  , testCase "REPL history preserves chronological numbering"
      testReplHistory
  , testCase "explicit RTS tuning reaches the application" testRtsOptions
  , testCase "backend help loads no environment" testBackendHelp
  , testCase "usage errors have a distinct exit status" testUsageErrors
  , testCase "backend options are validated before search" testOptionErrors
  , testCase "machine-sized options cannot wrap around" testIntOptionOverflow
  , testCase "Djinn renders definitions and expressions" testDjinnRendering
  , testCase "Djinn parses its shared class-context grammar" testDjinnContext
  , testCase "Djinn distinguishes refutation from truncation"
      testDjinnEvidence
  , testCase "Djinn selection preserves candidate-limit status"
      testDjinnSelection
  , testCase "Exference renders through the installed environment"
      testExferenceRendering
  , testCase "Exference accepts an empty checked environment"
      testEmptyExferenceEnvironment
  , testCase "Exference recursion helpers require explicit command opt-in"
      testExferenceRecursionPolicy
  , testCase "Exference environment IO failures stay structured"
      testMissingExferenceEnvironment
  , testCase "Exference rejects unsupported source vocabulary"
      testUnsupportedExferenceEnvironment
  , testCase "Exference parse failures are runtime diagnostics"
      testExferenceParseFailure
  , testCase "Exference resource bounds retain successful status"
      testExferenceBounds
  , testCase "Exference all-selection streams through terminal progress"
      testExferenceStreamingSelection
  , testCase "residual constraints remain attached to generated code"
      testResidualConstraintRendering
  ]

testGlobalInformation :: Assertion
testGlobalInformation = do
  (helpExit, help, helpErrors) <- runDjex ["--help"]
  assertEqual "help exit" ExitSuccess helpExit
  assertContains "global help" "djex djinn [OPTION...] TYPE" help
  assertContains "global help" "djex exference [OPTION...] TYPE" help
  assertContains "Djinn default follows its public options"
    ("positive proof-candidate limit (default: "
      ++ show (Djex.optionCutoff Djex.defaultQueryOptions) ++ ")") help
  assertContains "Djinn budget follows its public options"
    ("non-negative choice-point budget; 0 is unlimited (default: "
      ++ maybe "0" show (Djex.optionBudget Djex.defaultQueryOptions) ++ ")")
    help
  assertContains "Exference steps follow its public options"
    ("positive search-step limit (default: "
      ++ show (Djex.exferenceMaximumSteps Djex.defaultExferenceOptions) ++ ")")
    help
  assertContains "Exference queue follows its public options"
    ("non-negative queue limit or unbounded (default: "
      ++ maybe "unbounded" show
          (Djex.exferenceMaximumQueueSize Djex.defaultExferenceOptions)
      ++ ")") help
  assertEqual "help stderr" "" helpErrors

  (versionExit, versionOutput, versionErrors) <- runDjex ["--version"]
  assertEqual "version exit" ExitSuccess versionExit
  assertContains "version output" "djex version 2026.7.17" versionOutput
  assertEqual "version stderr" "" versionErrors

testReplDispatch :: Assertion
testReplDispatch = do
  (helpExit, help, helpErrors) <- runDjex ["repl", "--help"]
  assertEqual "REPL help exit" ExitSuccess helpExit
  assertContains "REPL help usage" "Usage: djex repl [OPTION...]" help
  assertContains "REPL backend startup option"
    "--backend=djinn|exference|both" help
  assertBool "REPL help must not initialize a session" $
    not $ "Djex REPL" `isInfixOf` help
  assertEqual "REPL help stderr" "" helpErrors

  (bareExit, bareOutput, _) <- runDjex []
  assertEqual "bare djex EOF exit" ExitSuccess bareExit
  assertContains "bare djex starts the REPL" "Djex REPL 2026.7.17" bareOutput

  assertUsageFailure ["repl", "--backend", "unknown"] "unknown backend"
  assertUsageFailure
    ["repl", "--backend", "djinn", "--backend", "both"]
    "--backend may be specified only once"
  assertUsageFailure ["repl", "unexpected"]
    "repl takes no positional arguments"

  withMissingPath $ \missingParent -> do
    (exitCode, output, errors) <- runDjex
      ["repl", "--history", missingParent ++ "/history"]
    assertEqual "missing history parent exit" (ExitFailure 1) exitCode
    assertEqual "missing history parent stdout" "" output
    assertContains "missing history parent diagnostic"
      "[DJEX_REPL_HISTORY_FILE]" errors

  withTemporaryEnvironment [] $ \directory -> do
    (exitCode, output, errors) <- runDjex
      ["repl", "--history", directory]
    assertEqual "directory history target exit" (ExitFailure 1) exitCode
    assertEqual "directory history target stdout" "" output
    assertContains "directory history target diagnostic"
      "history path names a directory" errors

testReplSharedSession :: Assertion
testReplSharedSession = withTemporaryEnvironment [] $ \directory -> do
  (exitCode, output, errors) <- runRepl directory
    [ ":set render expression"
    , "a -> a"
    , ":backend exference"
    , "a -> a"
    , ":djinn a -> a"
    , ":backend"
    , ":compare a -> a"
    ]
  assertEqual "shared REPL exit" ExitSuccess exitCode
  assertEqual "five independent identity results" 5
    $ countOccurrences "\\a -> a" output
  assertContains "backend switch" "Active backend: exference" output
  assertContains "explicit Djinn query did not switch backend"
    "exference\n" output
  assertContains "comparison labels Djinn" "-- Djinn" output
  assertContains "comparison labels Exference" "-- Exference" output
  assertBool "successful shared session emitted an error" $
    not $ "error" `isInfixOf` map toLower errors

testReplInputRecovery :: Assertion
testReplInputRecovery = withTemporaryEnvironment [] $ \directory -> do
  (exitCode, output, errors) <- runRepl directory
    [ ":set render expression"
    , ":{"
    , "a"
    , " -> a"
    , ":}"
    , ":"
    , ":c ignored"
    , ":wat"
    , ":! false"
    , "a -> a"
    ]
  assertEqual "recovering REPL exit" ExitSuccess exitCode
  assertEqual "multiline, repeat, and post-error results" 3
    $ countOccurrences "\\a -> a" output
  assertContains "ambiguous abbreviation diagnostic"
    "ambiguous command :c" errors
  assertContains "unknown command diagnostic" "unknown command :wat" errors
  assertContains "failed shell command diagnostic" "[DJEX_REPL_SHELL]" errors
  assertNoCallStack errors

testReplBackendIsolation :: Assertion
testReplBackendIsolation = withMissingPath $ \missing -> do
  (exitCode, output, errors) <- runDjexInput
    ["repl", "--backend", "both", "--environment", missing]
    $ unlines [":set prompt \"\"", "a -> a", ":quit"]
  assertEqual "isolated both-mode exit" ExitSuccess exitCode
  assertContains "Djinn section survives" "-- Djinn" output
  assertContains "Djinn result survives" "djexResult a = a" output
  assertContains "Exference section is still attempted" "-- Exference" output
  assertContains "missing environment diagnostic"
    "[DJEX_REPL_TARGET_NOT_FOUND]" errors
  assertContains "unavailable backend diagnostic"
    "[DJEX_REPL_EXFERENCE_UNAVAILABLE]" errors

testReplTransactionalLoad :: Assertion
testReplTransactionalLoad = withTemporaryEnvironment [] $ \directory ->
  withMissingPath $ \missing -> do
    (exitCode, output, errors) <- runRepl directory
      [ ":backend exference"
      , ":set render expression"
      , ":load " ++ show missing
      , "a -> a"
      , ":cd /"
      , ":reload"
      , "a -> a"
      , ":show environment"
      ]
    assertEqual "transactional load exit" ExitSuccess exitCode
    assertContains "failed replacement retains the old session"
      "retaining the previous session and settings" output
    assertEqual "old session remains usable across failed load and cwd change" 2
      $ countOccurrences "\\a -> a" output
    assertContains "reload retains canonical environment path" directory output
    assertContains "failed load reports missing source"
      "[DJEX_REPL_TARGET_NOT_FOUND]" errors

testReplWorkspaceTargets :: Assertion
testReplWorkspaceTargets = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      named = canonicalRoot </> "named"
      app = named </> "App.hs"
      dependency = named </> "Lib" </> "Dep.hs"
      directory = canonicalRoot </> "directory"
      first = directory </> "Dir" </> "First.hs"
      second = directory </> "Dir" </> "Second.hs"
      namedModules = unlines
        [ "Lib.Dep (" ++ dependency ++ ")"
        , "App (" ++ app ++ ")"
        ]
      directoryModules = unlines
        [ "Dir.First (" ++ first ++ ")"
        , "Dir.Second (" ++ second ++ ")"
        ]

  -- A module-name target is resolved against the admission directory, while
  -- reload retains its canonical file and dependency roots after :cd.
  (namedExit, namedOutput, namedErrors) <- runRepl empty
    [ ":cd " ++ show named
    , ":load App"
    , ":show targets"
    , ":show modules"
    , ":show imports"
    , ":cd /"
    , ":reload"
    , ":show targets"
    , ":show modules"
    ]
  assertEqual "named target REPL exit" ExitSuccess namedExit
  assertEqual "module-name target survives canonical reload" 2
    $ countOccurrences "App\n" namedOutput
  assertEqual "module-name dependency order survives reload" 2
    $ countOccurrences namedModules namedOutput
  assertContains "last named target becomes the automatic starred module"
    "import *App -- automatic" namedOutput
  assertNoCallStack namedErrors

  -- File and directory targets are displayed canonically. Replacing the
  -- target set also replaces its dependency closure, and bare :load unloads.
  (pathExit, pathOutput, pathErrors) <- runRepl empty
    [ ":load " ++ show app
    , ":show targets"
    , ":show modules"
    , ":load " ++ show directory
    , ":show targets"
    , ":show modules"
    , ":show imports"
    , ":load"
    , ":show targets"
    , ":show modules"
    , ":show imports"
    ]
  assertEqual "path target REPL exit" ExitSuccess pathExit
  assertContains "file target has canonical display" (app ++ "\n") pathOutput
  assertContains "file target dependencies are ordered first"
    namedModules pathOutput
  assertContains "directory target has canonical display"
    (directory ++ "\n") pathOutput
  assertContains "directory dependencies are ordered first"
    directoryModules pathOutput
  assertContains "first directory module enters the automatic context"
    "import *Dir.First -- automatic" pathOutput
  assertContains "every directory module enters the automatic context"
    "import *Dir.Second -- automatic" pathOutput
  assertContains "bare load clears explicit targets" "(no targets)" pathOutput
  assertContains "bare load clears their dependency closure"
    "(no modules loaded)" pathOutput
  assertContains "bare load clears the automatic context"
    "(no imports)" pathOutput
  assertNoCallStack pathErrors

testReplDefaultEnvironmentScope :: Assertion
testReplDefaultEnvironmentScope = do
  (exitCode, output, errors) <- runDjexInput
    ["repl", "--backend", "exference"] $ unlines
      [ ":set prompt \"\""
      , ":set render expression"
      , ":set select best"
      , ":set max-steps 16"
      , "a -> Data.Maybe.Maybe a"
      , ":quit"
      ]
  assertEqual "default environment REPL exit" ExitSuccess exitCode
  -- The first bounded result is Applicative.pure rather than Just; either is
  -- impossible in the historical broken state whose context held Data.Word
  -- alone. Keeping the bound small makes this startup regression inexpensive.
  assertContains
    ("default directory context retains non-final modules: " ++ output ++ errors)
    "Control.Applicative.pure" output
  assertBool "default environment lost its searchable module context" $
    not $ "DJEX_EXF_NO_RESULT" `isInfixOf` errors
  assertNoCallStack errors

testReplReloadTargetSpelling :: Assertion
testReplReloadTargetSpelling = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      target = canonicalRoot </> "rename" </> "Target.hs"
      replacement = canonicalRoot </> "rename" </> "After.source"
  (exitCode, output, errors) <- runRepl empty
    [ ":load " ++ show target
    , ":! cp " ++ show replacement ++ " " ++ show target
    , ":reload"
    , ":unadd Before"
    , ":unadd After"
    , ":show targets"
    ]
  assertEqual "renamed module reload REPL exit" ExitSuccess exitCode
  assertContains "reload accepts the replacement module declaration"
    "Loaded Exference environment:" output
  assertEqual "stale derived module spelling is rejected once" 1
    $ countOccurrences
        "Exference load failed; retaining the previous session and settings."
        output
  assertContains "stale derived module spelling is no longer loaded"
    "[DJEX_REPL_TARGET_NOT_LOADED]" errors
  assertContains "fresh derived module spelling removes the file target"
    "Removed Exference targets: \"After\"" output
  assertContains "fresh module spelling leaves no target"
    "(no targets)" output
  assertNoCallStack errors

testReplNamedSymlinkTargets :: Assertion
testReplNamedSymlinkTargets = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      aliasRoot = canonicalRoot </> "named-alias"
      original = aliasRoot </> "A.hs"
      alias = aliasRoot </> "B.hs"
  createFileLink original alias
  (exitCode, output, errors) <- runRepl empty
    [ ":cd " ++ show aliasRoot
    , ":load A B"
    , ":show modules"
    ]
  assertEqual "named symlink target REPL exit" ExitSuccess exitCode
  assertContains "conflicting named aliases reject the target transaction"
    "Exference load failed; retaining the previous session and settings."
    output
  assertContains "conflicting aliases retain the prior empty workspace"
    "(no modules loaded)" output
  assertContains "named alias mismatch has a structured diagnostic"
    "[DJEX_REPL_MODULE_MISMATCH]" errors
  assertContains "deduplication retains the second module expectation"
    "B was requested" errors
  assertContains "mismatch reports the canonical declaration" "declares A" errors
  assertNoCallStack errors

testReplDirectoryLinks :: Assertion
testReplDirectoryLinks = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      linkRoot = canonicalRoot </> "directory-links"
      linkedFile = canonicalRoot </> "linked-file" </> "Linked.hs"
      outsideSibling = canonicalRoot </> "linked-file" </> "OutsideSibling.hs"
      fileLink = linkRoot </> "Linked.hs"
      escapedDirectory = canonicalRoot </> "escaped-directory"
      escapeLink = linkRoot </> "Escape"
      cycleLink = linkRoot </> "Cycle"
      inside = linkRoot </> "Inside.hs"
  createFileLink linkedFile fileLink
  createDirectoryLink escapedDirectory escapeLink
  createDirectoryLink linkRoot cycleLink
  (exitCode, output, errors) <- runRepl empty
    [ ":load " ++ show linkRoot
    , ":show modules"
    , ":show imports"
    ]
  assertEqual "directory symlink REPL exit" ExitSuccess exitCode
  assertContains "ordinary source file in directory is loaded"
    ("Inside (" ++ inside ++ ")") output
  assertContains "regular-file symlink is admitted canonically"
    ("Linked (" ++ linkedFile ++ ")") output
  assertBool "directory symlink escaped the admitted source tree" $
    not $ "Escaped (" `isInfixOf` output
  assertBool "file symlink widened dependency discovery outside the tree" $
    not $ "OutsideSibling (" `isInfixOf` output
  assertContains "ordinary directory module enters automatic context"
    "import *Inside -- automatic" output
  assertContains "file symlink module enters automatic context"
    "import *Linked -- automatic" output
  assertContains "outside sibling import remains advisory"
    "[DJEX_REPL_IMPORT_UNRESOLVED]" errors
  assertNoCallStack errors

  (fileExit, fileOutput, fileErrors) <- runRepl empty
    [ ":load " ++ show linkedFile
    , ":show modules"
    ]
  assertEqual "explicit linked-file target REPL exit" ExitSuccess fileExit
  assertContains "explicit file target retains its hierarchical source root"
    ( "OutsideSibling (" ++ outsideSibling ++ ")\n"
      ++ "Linked (" ++ linkedFile ++ ")"
    ) fileOutput
  assertBool "explicit file target left its sibling unresolved" $
    not $ "DJEX_REPL_IMPORT_UNRESOLVED" `isInfixOf` fileErrors
  assertNoCallStack fileErrors

testReplTargetMutation :: Assertion
testReplTargetMutation = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      app = canonicalRoot </> "named" </> "App.hs"
      dependency = canonicalRoot </> "named" </> "Lib" </> "Dep.hs"
      missing = canonicalRoot </> "missing.hs"
      loadedModules = unlines
        [ "Lib.Dep (" ++ dependency ++ ")"
        , "App (" ++ app ++ ")"
        ]

  (removeExit, removeOutput, removeErrors) <- runRepl empty
    [ ":load " ++ show app
    , ":add " ++ show app
    , ":show targets"
    , ":unadd " ++ show app
    , ":show targets"
    , ":show modules"
    ]
  assertEqual "idempotent add REPL exit" ExitSuccess removeExit
  assertEqual "adding one canonical target twice retains one target" 1
    $ countOccurrences (app ++ "\n") removeOutput
  assertContains "unadding the sole target empties target state"
    "(no targets)" removeOutput
  assertContains "unadding a target prunes dependency-only modules"
    "(no modules loaded)" removeOutput
  assertNoCallStack removeErrors

  (failureExit, failureOutput, failureErrors) <- runRepl empty
    [ ":load " ++ show app
    , ":add " ++ show missing
    , ":show targets"
    , ":unadd " ++ show missing
    , ":show targets"
    , ":show modules"
    ]
  assertEqual "transactional target mutation exit" ExitSuccess failureExit
  assertEqual "both failed mutations report retained state" 2
    $ countOccurrences
        "Exference load failed; retaining the previous session and settings."
        failureOutput
  assertEqual "failed additions and removals retain the canonical target" 2
    $ countOccurrences (app ++ "\n") failureOutput
  assertContains "failed mutations retain the dependency closure"
    loadedModules failureOutput
  assertContains "missing addition is structured"
    "[DJEX_REPL_TARGET_NOT_FOUND]" failureErrors
  assertContains "unknown removal is structured"
    "[DJEX_REPL_TARGET_NOT_LOADED]" failureErrors
  assertNoCallStack failureErrors

testReplScopeRetention :: Assertion
testReplScopeRetention = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      alpha = canonicalRoot </> "scope" </> "Alpha.hs"
      beta = canonicalRoot </> "scope" </> "Beta.hs"
      surface = canonicalRoot </> "scope" </> "Surface.hs"

  (mutationExit, mutationOutput, mutationErrors) <- runRepl empty
    [ ":load " ++ show alpha
    , ":module Alpha"
    , ":add"
    , ":unadd"
    , ":show imports"
    , ":add " ++ show beta
    , ":show imports"
    , ":add " ++ show surface
    , ":show imports"
    , ":unadd " ++ show surface
    , ":show imports"
    , ":unadd " ++ show alpha
    , ":show imports"
    ]
  assertEqual "scope-resetting mutation REPL exit" ExitSuccess mutationExit
  assertEqual ":add selects its last newly contributing target" 1
    $ countOccurrences "import *Surface -- automatic\n" mutationOutput
  assertEqual ":unadd retains a surviving current target or falls back last" 3
    $ countOccurrences "import *Beta -- automatic\n" mutationOutput
  assertEqual "bare :add and :unadd preserve an explicit context" 1
    $ countOccurrences "import Alpha\n" mutationOutput
  assertNoCallStack mutationErrors

  (reloadExit, reloadOutput, reloadErrors) <- runRepl empty
    [ ":load " ++ show [alpha, beta]
    , ":module Alpha"
    , ":reload"
    , ":show imports"
    , ":module *Alpha"
    , ":reload"
    , ":show imports"
    , ":module"
    , "import Alpha"
    , ":reload"
    , ":show imports"
    ]
  assertEqual "scope-preserving reload REPL exit" ExitSuccess reloadExit
  assertEqual "plain modules and imports coexist with fresh automatic scope" 2
    $ countOccurrences
        "import Alpha\nimport *Alpha -- automatic\n" reloadOutput
  assertEqual "an exact explicit star suppresses the automatic duplicate" 1
    $ countOccurrences "import *Alpha\n" reloadOutput
  assertNoCallStack reloadErrors

testReplModuleContexts :: Assertion
testReplModuleContexts = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      alpha = canonicalRoot </> "scope" </> "Alpha.hs"
      beta = canonicalRoot </> "scope" </> "Beta.hs"
  (exitCode, output, errors) <- runRepl empty
    [ ":load " ++ show [alpha, beta]
    , ":show imports"
    , ":module Alpha"
    , ":module + Beta"
    , ":show imports"
    , ":module - Alpha"
    , ":module+ *Alpha"
    , ":show imports"
    , ":module+"
    , ":show imports"
    ]
  assertEqual "module context REPL exit" ExitSuccess exitCode
  assertEqual "first explicit load target supplies one automatic context" 1
    $ countOccurrences "import *Alpha -- automatic\n" output
  assertEqual "replacement context is retained through addition" 1
    $ countOccurrences "import Alpha\n" output
  assertEqual "spaced addition and subtraction update the context" 3
    $ countOccurrences "import Beta\n" output
  assertEqual "attached :module+ accepts stars and an empty no-op" 2
    $ countOccurrences "import *Alpha\n" output
  assertNoCallStack errors

testReplImportsAndBrowsing :: Assertion
testReplImportsAndBrowsing = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      surface = canonicalRoot </> "scope" </> "Surface.hs"
  (exitCode, output, errors) <- runRepl empty
    [ ":load " ++ show surface
    , ":module"
    , "import Surface"
    , "import qualified Surface"
    , "import Surface as S"
    , "import qualified Surface as Q (PublicType, publicValue)"
    , "import Surface hiding (publicValue)"
    , ":show imports"
    , ":browse Surface"
    , ":browse *Surface"
    ]
  assertEqual "import and browse REPL exit" ExitSuccess exitCode
  forM_
    [ "import Surface\n"
    , "import qualified Surface\n"
    , "import Surface as S\n"
    , "import qualified Surface as Q (PublicType, publicValue)\n"
    , "import Surface hiding (publicValue)\n"
    ] $ \declaration -> assertContains
      ("show imports retains " ++ show declaration) declaration output
  assertContains "normal browse is explicitly labelled"
    "-- Exference module Surface" output
  assertContains "starred browse is explicitly labelled"
    "-- Exference module *Surface" output
  assertEqual "ordinary browse hides a non-exported binding" 1
    $ countOccurrences "Surface.hiddenValue" output
  assertEqual "ordinary browse hides an unexported constructor" 1
    $ countOccurrences "Surface.HiddenConstructor" output
  assertEqual "both browse modes include an exported binding" 2
    $ countOccurrences "Surface.publicValue" output
  assertNoCallStack errors

testReplTypeInference :: Assertion
testReplTypeInference = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      typeSurface = canonicalRoot </> "type" </> "TypeSurface.hs"
      oversizedTuple = "(" ++ intercalate "," (replicate 65 "Ground") ++ ")"
  (exitCode, output, errors) <- runRepl empty
    [ ":load " ++ show typeSurface
    , ":set qualification none"
    , ":set render expression"
    , "a -> a"
    , ":t identity"
    , ":ty (identity)"
    , ":typ ((identity))"
    , ":type (((identity)))"
    , ":type \\x -> x"
    , ":type identity identity"
    , ":type \\x -> Pair x x"
    , ":type \\(Wrapped x) -> x"
    , ":type \\(%%) -> (%%)"
    , ":type (identity,)"
    , ":type \\case { Wrapped x -> x }"
    , ":type (Ground :: Ground)"
    , ":type \\(x :: Ground) -> x"
    , ":type ((,) :: a -> b -> (a, a))"
    , ":type (Ground :: a)"
    , ":type (identity :: Wrapped)"
    , ":type higher"
    , ":type identity <.> identity <.> identity"
    , ":type -identity <.> identity"
    , ":type \\(x :*: y :*: zs) -> x"
    , ":type " ++ oversizedTuple
    , ":type"
    , ":type +v identity"
    , ":"
    ]
  assertEqual "type-inference REPL exit" ExitSuccess exitCode
  forM_
    [ "identity :: a -> a"
    , "(identity) :: a -> a"
    , "((identity)) :: a -> a"
    , "(((identity))) :: a -> a"
    ] $ \rendered -> assertContains
      "alias and every unique type prefix dispatch identically" rendered output
  assertContains "lambda inference" "\\x -> x :: a -> a" output
  assertContains "polymorphic application inference"
    "identity identity :: a -> a" output
  assertContains "constructor application inference"
    "\\x -> Pair x x :: a -> Pair a a" output
  assertContains "constructor-pattern inference"
    "\\(Wrapped x) -> x :: Wrapped a -> a" output
  assertContains "symbolic pattern-variable inference"
    "\\(%%) -> (%%) :: a -> a" output
  assertContains "tuple-section inference"
    "(identity,) :: a -> (b -> b, a)" output
  assertContains "lambda-case inference"
    "\\case { Wrapped x -> x } :: Wrapped a -> a" output
  assertContains "ground expression annotation"
    "(Ground :: Ground) :: Ground" output
  assertContains "ground pattern annotation"
    "\\(x :: Ground) -> x :: Ground -> Ground" output
  assertEqual "unsound polymorphic annotations are rejected" 2
    $ countOccurrences "[DJEX_REPL_TYPE_ANNOTATION_UNSUPPORTED]" errors
  assertContains "ill-kinded ground annotations are rejected"
    "[DJEX_REPL_TYPE_ANNOTATION]" errors
  assertContains "higher-rank rendering keeps outer variables out of binders"
    "higher :: (forall b. b -> b) -> a -> a" output
  assertEqual "infix forms requiring unavailable fixities are rejected" 3
    $ countOccurrences "[DJEX_REPL_TYPE_UNSUPPORTED]" errors
  assertContains "oversized tuples are rejected before entering shared types"
    "[DJEX_REPL_TYPE_TUPLE]" errors
  assertEqual ":type does not replace the last synthesis query" 2
    $ countOccurrences "\\a -> a" output
  assertEqual "missing expression and obsolete mode are both command errors" 2
    $ countOccurrences "[DJEX_REPL_COMMAND]" errors
  assertContains "missing-expression diagnostic"
    "expected a Haskell expression" errors
  assertContains "obsolete +v diagnostic"
    "`:type +v' has gone; use `:type' instead" errors
  assertNoCallStack errors

testReplTypeScope :: Assertion
testReplTypeScope = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      typeSurface = canonicalRoot </> "type" </> "TypeSurface.hs"
  (exitCode, output, errors) <- runRepl empty
    [ ":load " ++ show typeSurface
    , ":set qualification none"
    , ":type Pair"
    , ":type Wrapped"
    , ":type convert"
    , ":type (<.>)"
    , ":module"
    , "import qualified TypeSurface as T"
    , ":type T.identity"
    , ":type T.Pair"
    , ":type T.convert"
    , ":type +d T.number"
    , ":type identity"
    , ":type T.Convert"
    ]
  assertEqual "type-scope REPL exit" ExitSuccess exitCode
  assertContains "same-spelled binary constructor has a term signature"
    "Pair :: a -> b -> Pair a b" output
  assertContains "same-spelled unary constructor has a term signature"
    "Wrapped :: a -> Wrapped a" output
  assertContains "class methods retain their implicit owner constraint"
    "convert :: Convert a => a -> Wrapped a" output
  assertContains "operator signatures are inspectable in prefix form"
    "(<.>) :: (a -> b) -> a -> b" output
  assertContains "qualified alias resolves an ordinary value"
    "T.identity :: a -> a" output
  assertContains "qualified alias resolves a constructor"
    "T.Pair :: a -> b -> Pair a b" output
  assertContains "qualified alias resolves a class method"
    "T.convert :: Convert a => a -> Wrapped a" output
  assertContains "defaulting distinguishes a user-defined Num class"
    "T.number :: Num a => a" output
  assertEqual "out-of-scope value and type-only occurrence are rejected" 2
    $ countOccurrences "[DJEX_REPL_TYPE_SCOPE]" errors
  assertContains "unqualified value is unavailable after clearing its module"
    "identity" errors
  assertContains "a class name is not accepted in the term namespace"
    "Convert" errors
  assertNoCallStack errors

testReplTypeDefaulting :: Assertion
testReplTypeDefaulting = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      prelude = canonicalRoot </> "type" </> "Prelude.hs"
  (exitCode, output, errors) <- runRepl empty
    [ ":load " ++ show prelude
    , ":set qualification none"
    , ":type 1"
    , ":type +d 1"
    , ":type 1.5"
    , ":type +d 1.5"
    , ":type [1,2..3]"
    , ":type +d [1,2..3]"
    , ":type [1,2.0]"
    , ":type \\1 -> ()"
    , ":type Text.Show.show 1"
    ]
  assertEqual "type-defaulting REPL exit" ExitSuccess exitCode
  assertContains "ordinary integral literal remains polymorphic"
    "1 :: Num a => a" output
  assertContains "+d selects Integer for an integral literal"
    "1 :: Integer" output
  assertContains "ordinary fractional literal remains polymorphic"
    "1.5 :: Fractional a => a" output
  assertContains "+d selects Double for a fractional literal"
    "1.5 :: Double" output
  assertContains "enumeration inference is independent of method spellings"
    "[1,2..3] :: (Enum a, Num a) => [a]" output
  assertContains "+d defaults an enumerated element type"
    "[1,2..3] :: [Integer]" output
  assertContains "superclass-redundant constraints are removed"
    "[1,2.0] :: Fractional a => [a]" output
  assertContains "numeric patterns require equality"
    "\\1 -> () :: (Eq a, Num a) => a -> ()" output
  assertContains "ordinary defaulting tests loaded candidate evidence"
    "Text.Show.show 1 :: Text" output
  assertNoCallStack errors

testReplSearchScope :: Assertion
testReplSearchScope = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      alpha = canonicalRoot </> "scope" </> "Alpha.hs"
      beta = canonicalRoot </> "scope" </> "Beta.hs"

  -- Clearing the source context must not remove syntax-level constructors.
  -- Conversely, it must not leave ordinary loaded source declarations usable.
  (structuralExit, structuralOutput, structuralErrors) <- runRepl empty
    [ ":load " ++ show alpha
    , ":backend exference"
    , ":set render expression"
    , ":set max-steps 8"
    , ":module"
    , "a -> [a]"
    , "Alpha.AlphaType"
    ]
  assertEqual "structural scope REPL exit" ExitSuccess structuralExit
  assertContains "list constructors remain searchable in an empty context"
    "[]" structuralOutput
  assertBool "empty context leaked an ordinary source binding" $
    not $ "Alpha.alphaValue" `isInfixOf` structuralOutput
  assertContains "empty context cannot synthesize an ordinary source type"
    "[DJEX_EXF_NO_RESULT]" structuralErrors

  -- The searched dictionary and the query type resolver use the same import
  -- projection: an unimported module and a name omitted by an alias list are
  -- rejected, while the selected qualified binding is actually synthesized.
  (scopeExit, scopeOutput, scopeErrors) <- runRepl empty
    [ ":load " ++ show [alpha, beta]
    , ":backend exference"
    , ":set render expression"
    , ":set max-steps 4"
    , ":module Alpha"
    , "Beta.BetaType"
    , ":module"
    , "import qualified Beta as B (BetaType, betaValue)"
    , "B.BetaType"
    , "B.OtherType"
    , ":module"
    , "import Beta hiding (otherValue)"
    , "OtherType"
    ]
  assertEqual "scoped search REPL exit" ExitSuccess scopeExit
  assertEqual "selected qualified import contributes one searchable binding" 1
    $ countOccurrences "Beta.betaValue" scopeOutput
  assertBool "hiding leaked the uniquely typed binding" $
    not $ "Beta.otherValue" `isInfixOf` scopeOutput
  assertContains "unimported and list-excluded query types are rejected"
    "[DJEX_EXF_PARSE]" scopeErrors
  assertContains "hidden unique binding produces no result"
    "[DJEX_EXF_NO_RESULT]" scopeErrors
  assertNoCallStack scopeErrors

testReplAliasesAndReexports :: Assertion
testReplAliasesAndReexports = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      reexport = canonicalRoot </> "reexport" </> "Reexport.hs"
      aliasLeft = canonicalRoot </> "scope" </> "AliasLeft.hs"
      aliasRight = canonicalRoot </> "scope" </> "AliasRight.hs"
      querySetup targets =
        [ ":load " ++ show targets
        , ":backend exference"
        , ":set render expression"
        , ":set max-steps 4"
        , ":module"
        ]

  (reexportExit, reexportOutput, reexportErrors) <- runRepl empty $
    querySetup [reexport]
      ++ [ "import qualified Reexport as X"
         , "X.Item"
         ]
  assertEqual "re-export alias REPL exit" ExitSuccess reexportExit
  assertContains "alias resolves a re-export to its defining module"
    "Origin.itemValue" reexportOutput
  assertNoCallStack reexportErrors

  (disjointExit, disjointOutput, disjointErrors) <- runRepl empty $
    querySetup [aliasLeft, aliasRight]
      ++ [ "import qualified AliasLeft as X (LeftType, leftValue)"
         , "import qualified AliasRight as X (RightType, rightValue)"
         , "X.LeftType"
         , "X.RightType"
         ]
  assertEqual "disjoint shared alias REPL exit" ExitSuccess disjointExit
  assertContains "shared alias resolves its left-only occurrence"
    "AliasLeft.leftValue" disjointOutput
  assertContains "shared alias resolves its right-only occurrence"
    "AliasRight.rightValue" disjointOutput
  assertBool "disjoint aliases were rejected eagerly" $
    not $ "DJEX_REPL_IMPORT_ALIAS_AMBIGUOUS" `isInfixOf` disjointErrors
  assertNoCallStack disjointErrors

  (overlapExit, overlapOutput, overlapErrors) <- runRepl empty $
    querySetup [aliasLeft, aliasRight]
      ++ [ "import qualified AliasLeft as X (Clash, leftClash)"
         , "import qualified AliasRight as X (Clash, rightClash)"
         , ":show imports"
         , "X.Clash"
         ]
  assertEqual "overlapping shared alias REPL exit" ExitSuccess overlapExit
  assertContains "first overlapping import is retained"
    "import qualified AliasLeft as X (Clash, leftClash)" overlapOutput
  assertContains "second overlapping import is retained"
    "import qualified AliasRight as X (Clash, rightClash)" overlapOutput
  assertBool "overlapping alias leaked its left candidate" $
    not $ "AliasLeft.leftClash" `isInfixOf` overlapOutput
  assertBool "overlapping alias leaked its right candidate" $
    not $ "AliasRight.rightClash" `isInfixOf` overlapOutput
  assertContains "shared occurrence ambiguity is rejected when used"
    "[DJEX_EXF_PARSE]" overlapErrors
  assertContains "shared occurrence diagnostic explains the ambiguity"
    "ambiguous" $ map toLower overlapErrors
  assertNoCallStack overlapErrors

testReplUnresolvedImportList :: Assertion
testReplUnresolvedImportList = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      unresolved = canonicalRoot </> "unresolved" </> "UnresolvedList.hs"
  (exitCode, output, errors) <- runRepl empty
    [ ":load " ++ show unresolved
    , ":show modules"
    , ":show diagnostics"
    ]
  assertEqual "unresolved import-list REPL exit" ExitSuccess exitCode
  assertContains "unresolved import list does not abort the load transaction"
    "Loaded Exference environment:" output
  assertContains "module with unresolved import list is committed"
    "UnresolvedList (" output
  assertBool "unresolved import list was treated as a fatal load error" $
    not $ "Exference load failed" `isInfixOf` output
  assertContains "unresolved import warning is retained by :show diagnostics"
    "[DJEX_REPL_IMPORT_UNRESOLVED]" output
  assertContains "unresolved import warning is emitted when loading"
    "[DJEX_REPL_IMPORT_UNRESOLVED]" errors
  assertBool "unresolved import list leaked a fatal item diagnostic" $
    not $ "DJEX_REPL_IMPORT_NAME" `isInfixOf` errors
  assertNoCallStack errors

testReplRecordSelectors :: Assertion
testReplRecordSelectors = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      records = canonicalRoot </> "scope" </> "RecordSurface.hs"
  (exitCode, output, errors) <- runRepl empty
    [ ":load " ++ show records
    , ":backend exference"
    , ":set render expression"
    , ":set max-steps 4"
    , ":module"
    , "import qualified RecordSurface as R (Record(field), Field)"
    , ":show imports"
    , "R.Record -> R.Field"
    , ":module"
    , "import RecordSurface hiding (field)"
    , "Record -> Field"
    ]
  assertEqual "record selector REPL exit" ExitSuccess exitCode
  assertContains "record child survives module export and explicit import"
    "import qualified RecordSurface as R (Record(field), Field)" output
  assertEqual "selected record field is the sole projected inhabitant" 1
    $ countOccurrences "RecordSurface.field" output
  assertContains "hiding a record field removes it from search"
    "[DJEX_EXF_NO_RESULT]" errors
  assertNoCallStack errors

testReplAbstractRecordExports :: Assertion
testReplAbstractRecordExports = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      abstractRecord = canonicalRoot </> "abstract-record" </> "A.hs"
  (exitCode, output, errors) <- runRepl empty
    [ ":load " ++ show abstractRecord
    , ":backend exference"
    , ":set render expression"
    , ":set max-steps 4"
    , ":module"
    , "import A (T(..))"
    , ":show imports"
    , "import A (T(field))"
    , ":show imports"
    , "import A (T, Field, field)"
    , ":show imports"
    , "T -> Field"
    ]
  assertEqual "abstract record-export REPL exit" ExitSuccess exitCode
  assertEqual "bundled imports of an abstract type both roll back" 2
    $ countOccurrences "(no imports)" output
  assertContains "separately exported type and field can be imported explicitly"
    "import A (T, Field, field)" output
  assertContains "successful explicit import makes the selector searchable"
    "A.field" output
  assertBool "abstract bundled imports produced no structured diagnostics" $
    countOccurrences "[DJEX_REPL_IMPORT_" errors >= 2
  assertNoCallStack errors

testReplSourcePackageImport :: Assertion
testReplSourcePackageImport = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      localModule = canonicalRoot </> "package-import" </> "LocalM.hs"
      packageUser = canonicalRoot </> "package-import" </> "PackageUser.hs"
  (exitCode, output, errors) <- runRepl empty
    [ ":load " ++ show [packageUser, localModule]
    , ":show targets"
    , ":show modules"
    ]
  assertEqual "source package import REPL exit" ExitSuccess exitCode
  assertContains "package import rejects the whole target transaction"
    "Exference load failed; retaining the previous session and settings."
    output
  assertContains "failed source package import retains the empty workspace"
    "(no modules loaded)" output
  assertBool "package import accidentally committed its same-named local target" $
    not $ packageUser `isInfixOf` output
  assertContains ("source package import has a structured diagnostic: " ++ errors)
    "[DJEX_REPL_IMPORT_PACKAGE]" errors
  assertNoCallStack errors

testReplSymlinkModuleMismatch :: Assertion
testReplSymlinkModuleMismatch = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      symlinkRoot = canonicalRoot </> "symlink"
      realModule = symlinkRoot </> "Real.hs"
      aliasModule = symlinkRoot </> "Alias.hs"
      importer = symlinkRoot </> "UsesAlias.hs"
  createFileLink realModule aliasModule
  (exitCode, output, errors) <- runRepl empty
    [ ":load " ++ show [realModule, importer]
    , ":show modules"
    ]
  assertEqual "symlink module mismatch REPL exit" ExitSuccess exitCode
  assertContains "symlink mismatch rejects the target transaction"
    "Exference load failed; retaining the previous session and settings."
    output
  assertContains "symlink mismatch retains the prior empty workspace"
    "(no modules loaded)" output
  assertContains "symlink alias mismatch has a structured diagnostic"
    "[DJEX_REPL_MODULE_MISMATCH]" errors
  assertContains "symlink mismatch names the imported module" "Alias" errors
  assertContains "symlink mismatch names the declared module" "Real" errors
  assertNoCallStack errors

testReplExportAmbiguity :: Assertion
testReplExportAmbiguity = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      exportRoot = canonicalRoot </> "export-scope"
      duplicate = exportRoot </> "Duplicate.hs"
      namespace = exportRoot </> "Namespace.hs"
      collision = exportRoot </> "Collision.hs"

  (allowedExit, allowedOutput, allowedErrors) <- runRepl empty
    [ ":load " ++ show duplicate
    , ":show imports"
    , ":load " ++ show namespace
    , ":show imports"
    ]
  assertEqual "allowed re-export REPL exit" ExitSuccess allowedExit
  assertContains "re-exporting the same canonical name twice is harmless"
    "import *Duplicate -- automatic" allowedOutput
  assertContains
    ("type and value namespaces may share an occurrence: "
      ++ allowedOutput ++ allowedErrors)
    "import *Namespace -- automatic" allowedOutput
  assertBool "valid re-exports were diagnosed as ambiguous" $
    not $ "DJEX_REPL_EXPORT_AMBIGUOUS" `isInfixOf` allowedErrors
  assertNoCallStack allowedErrors

  (collisionExit, collisionOutput, collisionErrors) <- runRepl empty
    [ ":load " ++ show collision
    , ":show modules"
    ]
  assertEqual "ambiguous re-export REPL exit" ExitSuccess collisionExit
  assertContains "colliding re-export rejects the target transaction"
    "Exference load failed; retaining the previous session and settings."
    collisionOutput
  assertContains "colliding re-export retains the prior empty workspace"
    "(no modules loaded)" collisionOutput
  assertContains "same-namespace collision has a structured diagnostic"
    "[DJEX_REPL_EXPORT_AMBIGUOUS]" collisionErrors
  assertNoCallStack collisionErrors

testReplBundledOwners :: Assertion
testReplBundledOwners = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      bundleRoot = canonicalRoot </> "bundle"
      emptyData = bundleRoot </> "EmptyData.hs"
      invalidSynonym = bundleRoot </> "InvalidSynonym.hs"
  (exitCode, output, errors) <- runRepl empty
    [ ":load " ++ show emptyData
    , ":module"
    , "import EmptyData (E(..))"
    , ":show imports"
    , ":load " ++ show invalidSynonym
    , ":show modules"
    ]
  assertEqual "bundled owner REPL exit" ExitSuccess exitCode
  assertContains "an empty datatype owns a valid empty wildcard bundle"
    "import EmptyData (E(..))" output
  assertContains "type-synonym wildcard rejects the target transaction"
    "Exference load failed; retaining the previous session and settings."
    output
  assertContains "invalid synonym export retains the prior empty datatype"
    ("EmptyData (" ++ emptyData ++ ")") output
  assertBool "invalid synonym export was committed" $
    not $ "InvalidSynonym (" `isInfixOf` output
  assertContains "invalid wildcard diagnostic identifies its source form"
    "S(..)" errors
  assertNoCallStack errors

testReplImportRollback :: Assertion
testReplImportRollback = withReplModuleFixture $ \root -> do
  canonicalRoot <- canonicalizePath root
  let empty = canonicalRoot </> "empty"
      beta = canonicalRoot </> "scope" </> "Beta.hs"
  (exitCode, output, errors) <- runRepl empty
    [ ":load " ++ show beta
    , ":module Beta"
    , ":backend djinn"
    , ":set render expression"
    , "a -> a"
    , "import qualified Beta as"
    , ":show imports"
    , "import Missing"
    , ":show imports"
    , "import \"package-name\" Beta"
    , ":show imports"
    , "a -> a"
    ]
  assertEqual "import rollback REPL exit" ExitSuccess exitCode
  assertEqual "each failed import retains the prior module context" 3
    $ countOccurrences "import Beta\n" output
  assertContains "malformed import has a dedicated diagnostic"
    "[DJEX_REPL_IMPORT_PARSE]" errors
  assertContains "missing module import has a dedicated diagnostic"
    "[DJEX_REPL_MODULE_NOT_LOADED]" errors
  assertContains "package import has a dedicated diagnostic"
    "[DJEX_REPL_IMPORT_PACKAGE]" errors
  assertEqual "Exference scope edits do not alter the Djinn session" 2
    $ countOccurrences "\\a -> a" output
  assertNoCallStack errors

testReplFixReload :: Assertion
testReplFixReload = withTemporaryEnvironment
    [("Fix.hs", unlines
      [ "module Data.Function where"
      , "fix :: (a -> a) -> a"
      ])] $ \directory -> do
  (exitCode, output, errors) <- runRepl directory
    [ ":backend exference"
    , ":set render expression"
    , ":set max-steps 8"
    , "a"
    , ":set +fix"
    , "a"
    ]
  assertEqual "fix reload exit" ExitSuccess exitCode
  assertContains "policy change reloads the source environment"
    "Loaded Exference environment:" output
  assertEqual "fix is introduced only after opt-in" 1
    $ countOccurrences "Data.Function.fix" output
  assertContains "safe policy finds no unrestricted inhabitant first"
    "[DJEX_EXF_NO_RESULT]" errors

testReplScripts :: Assertion
testReplScripts = withTemporaryEnvironment [] $ \directory -> do
  let script = directory ++ "/commands.djex"
      recursive = directory ++ "/recursive.djex"
      broken = directory ++ "/broken.djex"
  writeFile script $ unlines
    [ ":set render expression"
    , ":backend exference"
    , "a -> a"
    ]
  writeFile recursive $ ":script " ++ show recursive ++ "\n"
  writeFile broken $ unlines ["", ":wat"]
  (exitCode, output, errors) <- runRepl directory
    [ ":script " ++ show script
    , ":backend"
    , ":script " ++ show recursive
    , ":script " ++ show broken
    , "a -> a"
    ]
  assertEqual "script REPL exit" ExitSuccess exitCode
  assertEqual "script and recovered interactive result" 2
    $ countOccurrences "\\a -> a" output
  assertContains "script setting persists" "exference\n" output
  assertContains "recursive script is rejected"
    "[DJEX_REPL_SCRIPT_CYCLE]" errors
  assertContains "script command diagnostic keeps its source line"
    (broken ++ " (line 2)") errors

testReplHistory :: Assertion
testReplHistory = withTemporaryEnvironment [] $ \directory -> do
  let history = directory ++ "/history"
      script = directory ++ "/history.djex"
  -- Haskeline persists newest-first, matching 'historyLines'. The driver
  -- reverses that representation before assigning chronological line numbers.
  writeFile history "old-two\nold-one\n"
  writeFile script ":history 1\n"
  (exitCode, output, errors) <- runDjexInput
    ["repl", "--environment", directory, "--history", history]
    $ unlines [":set prompt \"\"", ":script " ++ show script, ":quit"]
  assertEqual "history REPL exit" ExitSuccess exitCode
  assertContains "script sees latest session history entry" "2  old-two" output
  assertBool "history selected the oldest entry" $
    not $ "1  old-one" `isInfixOf` output
  assertBool "history session emitted an error" $
    not $ "error" `isInfixOf` map toLower errors

testRtsOptions :: Assertion
testRtsOptions = do
  (exitCode, output, errors) <-
    runDjex ["+RTS", "-K64m", "-RTS", "--version"]
  assertEqual "RTS-tuned version exit" ExitSuccess exitCode
  assertContains "application ran after RTS parsing"
    "djex version 2026.7.17" output
  assertEqual "RTS-tuned version stderr" "" errors

testBackendHelp :: Assertion
testBackendHelp = do
  forM_ ["djinn", "exference"] $ \backend -> do
    (exitCode, output, errors) <- runDjex [backend, "--help"]
    assertEqual (backend ++ " help exit") ExitSuccess exitCode
    assertContains (backend ++ " help")
      ("Usage: djex " ++ backend ++ " [OPTION...] TYPE") output
    assertEqual (backend ++ " help stderr") "" errors

testUsageErrors :: Assertion
testUsageErrors = do
  assertUsageFailure ["unknown", "a -> a"] "unknown command"
  assertUsageFailure ["djinn"] "exactly one input type is required"
  assertUsageFailure ["djinn", "a", "b"]
    "exactly one input type is required, but got 2"
  assertUsageFailure ["djinn", "--max-steps", "1", "a -> a"]
    "unrecognized option"
  assertUsageFailure ["exference", "--choice-budget", "1", "a -> a"]
    "unrecognized option"

testOptionErrors :: Assertion
testOptionErrors = do
  assertUsageFailure
    ["djinn", "--candidate-limit", "0", "a -> a"]
    "--candidate-limit must be a positive integer"
  assertUsageFailure
    ["djinn", "--target", "Data.result", "a -> a"]
    "invalid --target"
  assertUsageFailure
    ["exference", "--target", "Data.result", "("]
    "invalid --target"
  assertUsageFailure
    ["exference", "--max-depth", "NaN", "a -> a"]
    "--max-depth must be a finite non-negative number"
  assertUsageFailure
    ["exference", "--allow-unused", "--allow-unused", "a -> a"]
    "--allow-unused may be specified only once"
  assertUsageFailure
    ["exference", "--fix", "--fix", "a -> a"]
    "--fix may be specified only once"

testIntOptionOverflow :: Assertion
testIntOptionOverflow = do
  let wrappedZero = show intModulus
      wrappedOne = show $ intModulus + 1
  assertUsageFailure
    ["djinn", "--candidate-limit", wrappedOne, "a -> a"]
    "--candidate-limit must be a positive integer"
  assertUsageFailure
    [ "exference"
    , "--constraint-deferral-steps", wrappedZero
    , "a -> a"
    ]
    "--constraint-deferral-steps must be a non-negative integer"
  assertUsageFailure
    ["exference", "--max-steps", wrappedOne, "a -> a"]
    "--max-steps must be a positive integer"
  assertUsageFailure
    ["exference", "--max-queue", wrappedZero, "a -> a"]
    "--max-queue must be a non-negative integer"

-- Values at these offsets previously became zero or one when read as Int.
-- Deriving the modulus from the host bounds keeps the regression portable.
intModulus :: Integer
intModulus =
  toInteger (maxBound :: Int) - toInteger (minBound :: Int) + 1

testDjinnRendering :: Assertion
testDjinnRendering = do
  definition <- assertSuccess ["djinn", "a -> a"]
  assertEqual "Djinn definition" "djexResult a = a\n" definition

  expression <- assertSuccess
    ["djinn", "--render", "expression", "a -> a"]
  assertEqual "Djinn expression must reconstruct its leading lambda"
    "\\a -> a\n" expression

testDjinnContext :: Assertion
testDjinnContext = do
  output <- assertSuccess ["djinn", "Eq a => a -> a"]
  assertContains "contextual Djinn result" "djexResult a = a" output

testDjinnEvidence :: Assertion
testDjinnEvidence = do
  (negativeExit, negativeOutput, negativeErrors) <- runDjex
    ["djinn", "((a -> b) -> a) -> a"]
  assertEqual "logical refutation exit" ExitSuccess negativeExit
  assertEqual "logical refutation stdout" "" negativeOutput
  assertContains "logical refutation diagnostic"
    "[DJEX_DJINN_UNINHABITABLE]" negativeErrors
  assertBool "a refutation must not be described as undecided" $
    not $ "DJEX_DJINN_UNDECIDED" `isInfixOf` negativeErrors

  (boundedExit, boundedOutput, boundedErrors) <- runDjex
    ["djinn", "--choice-budget", "1", "((a -> b) -> a) -> a"]
  assertEqual "bounded search exit" ExitSuccess boundedExit
  assertEqual "bounded search stdout" "" boundedOutput
  assertContains "bounded search evidence" "[DJEX_DJINN_UNDECIDED]"
    boundedErrors
  assertContains "bounded search status" "[DJEX_SEARCH_TRUNCATED]"
    boundedErrors

testDjinnSelection :: Assertion
testDjinnSelection = do
  (limitedExit, limitedOutput, limitedErrors) <- runDjex
    [ "djinn"
    , "--select", "all"
    , "--candidate-limit", "1"
    , "a -> a -> a"
    ]
  assertEqual "candidate-limited exit" ExitSuccess limitedExit
  assertContains "candidate retained across truncation" "djexResult"
    limitedOutput
  assertContains "candidate-limit warning" "CandidateLimitReached"
    limitedErrors

  allOutput <- assertSuccess
    ["djinn", "--select", "all", "a -> a -> a"]
  assertContains "all mode candidate separator" "-- or" allOutput

testExferenceRendering :: Assertion
testExferenceRendering = do
  definition <- assertSuccess
    ["exference", "--select", "first", "a -> a"]
  assertEqual "Exference promoted definition" "djexResult a = a\n" definition

  expression <- assertSuccess
    [ "exference", "--select", "first"
    , "--render", "expression"
    , "a -> a"
    ]
  assertEqual "Exference expression" "\\a -> a\n" expression

testEmptyExferenceEnvironment :: Assertion
testEmptyExferenceEnvironment = withTemporaryEnvironment [] $ \directory -> do
  output <- assertSuccess
    [ "exference", "--environment", directory
    , "--select", "first"
    , "--render", "expression"
    , "a -> a"
    ]
  assertEqual "environment-free lambda" "\\a -> a\n" output

testExferenceRecursionPolicy :: Assertion
testExferenceRecursionPolicy = withTemporaryEnvironment
    [("Fix.hs", unlines
      [ "module Data.Function where"
      , "fix :: (a -> a) -> a"
      ])] $ \directory -> do
  let common =
        [ "exference"
        , "--environment", directory
        , "--select", "first"
        , "--render", "expression"
        , "--max-steps", "8"
        ]
      goal = "a"
  (defaultExit, defaultOutput, defaultErrors) <- runDjex $ common ++ [goal]
  assertEqual "safe default exit" ExitSuccess defaultExit
  assertEqual "safe default output" "" defaultOutput
  assertContains "safe default no-result diagnostic"
    "[DJEX_EXF_NO_RESULT]" defaultErrors
  assertBool "safe default admitted Data.Function.fix" $
    not $ "Data.Function.fix" `isInfixOf` defaultOutput

  allowedOutput <- assertSuccess $ common ++ ["--fix", goal]
  assertContains "explicit recursion-helper result"
    "Data.Function.fix" allowedOutput

testMissingExferenceEnvironment :: Assertion
testMissingExferenceEnvironment = withMissingPath $ \path -> do
  (exitCode, output, errors) <- runDjex
    ["exference", "--environment", path, "a -> a"]
  assertEqual "missing environment exit" (ExitFailure 1) exitCode
  assertEqual "missing environment stdout" "" output
  assertContains "missing environment code"
    "[EXF_ENV_DIRECTORY_READ]" errors
  assertContains "missing environment path" path errors
  assertNoCallStack errors

testUnsupportedExferenceEnvironment :: Assertion
testUnsupportedExferenceEnvironment = withTemporaryEnvironment
    [("Broken.hs", unlines
      [ "{-# LANGUAGE TypeFamilies #-}"
      , "module Broken where"
      , "type family F a"
      ])] $ \directory -> do
  (exitCode, output, errors) <- runDjex
    ["exference", "--environment", directory, "a -> a"]
  assertEqual "unsupported environment exit" (ExitFailure 1) exitCode
  assertEqual "unsupported environment stdout" "" output
  assertContains "unsupported vocabulary code"
    "[EXF_UNSUPPORTED_VOCABULARY]" errors
  assertNoCallStack errors

testExferenceParseFailure :: Assertion
testExferenceParseFailure = do
  (exitCode, output, errors) <- runDjex
    ["exference", "--select", "first", "("]
  assertEqual "parse failure exit" (ExitFailure 1) exitCode
  assertEqual "parse failure stdout" "" output
  assertContains "parse failure code" "[DJEX_EXF_PARSE]" errors
  assertContains "parse failure source" "<command-line>" errors
  assertNoCallStack errors

testExferenceBounds :: Assertion
testExferenceBounds = withTemporaryEnvironment [] $ \directory -> do
  (exitCode, output, errors) <- runDjex
    [ "exference", "--environment", directory
    , "--select", "first"
    , "--max-steps", "1"
    , "--max-queue", "0"
    , "a -> a"
    ]
  assertEqual "bounded Exference exit" ExitSuccess exitCode
  assertEqual "bounded Exference stdout" "" output
  assertContains "bounded Exference no-result diagnostic"
    "[DJEX_EXF_NO_RESULT]" errors
  assertContains "bounded Exference truncation diagnostic"
    "[DJEX_SEARCH_TRUNCATED]" errors

testExferenceStreamingSelection :: Assertion
testExferenceStreamingSelection =
    withTemporaryEnvironment [] $ \directory -> do
  (exitCode, output, errors) <- runDjex
    [ "exference", "--environment", directory
    , "--select", "all"
    , "--max-steps", "5"
    , "(a -> a) -> a -> a"
    ]
  assertEqual "streaming all-selection exit" ExitSuccess exitCode
  assertEqual "streaming all-selection separators"
    ( "djexResult f1 = f1\n\n-- or\n\n"
      ++ "djexResult f1 b = f1 (f1 b)\n"
    ) output
  assertContains "streaming terminal truncation"
    "[DJEX_SEARCH_TRUNCATED]" errors

testResidualConstraintRendering :: Assertion
testResidualConstraintRendering = withTemporaryEnvironment
    [("Fixture.hs", unlines
      [ "module Fixture where"
      , "data Box a = Box a"
      , "class C a"
      , "(<+>) :: C (Box a) => Box a"
      ])] $ \directory -> do
  unqualified <- search directory "none"
  assertContains "unqualified residual"
    "-- requires: C (Box a)" unqualified
  assertContains "unqualified operator" "(<+>)" unqualified
  assertBool "none qualification retained a global module prefix" $
    not $ "Fixture." `isInfixOf` unqualified

  identifiers <- search directory "identifiers"
  assertContains "identifier-qualified residual"
    "-- requires: Fixture.C (Fixture.Box a)" identifiers
  assertContains "middle-policy operator" "(<+>)" identifiers
  assertBool "middle qualification unexpectedly qualified an operator" $
    not $ "Fixture.<+>" `isInfixOf` identifiers

  fullyQualified <- search directory "full"
  assertContains "fully qualified residual"
    "-- requires: Fixture.C (Fixture.Box a)" fullyQualified
  assertContains "fully qualified operator" "(Fixture.<+>)" fullyQualified
 where
  search directory qualification = do
    (exitCode, output, errors) <- runDjex
      [ "exference", "--environment", directory
      , "--select", "first"
      , "--allow-constraints"
      , "--qualification", qualification
      , "--render", "expression"
      , "--max-steps", "2048"
      , "Fixture.Box a"
      ]
    assertEqual ("residual search stderr: " ++ errors) ExitSuccess exitCode
    assertBool "residual constraints must not be detached as diagnostics" $
      not $ "DJEX_EXF_RESIDUAL_CONSTRAINTS" `isInfixOf` errors
    pure output

assertSuccess :: [String] -> IO String
assertSuccess arguments = do
  (exitCode, output, errors) <- runDjex arguments
  assertEqual ("djex stderr: " ++ errors) ExitSuccess exitCode
  assertBool ("successful synthesis emitted an error: " ++ errors) $
    not $ "error" `isInfixOf` map toLower errors
  pure output

assertUsageFailure :: [String] -> String -> Assertion
assertUsageFailure arguments expected = do
  (exitCode, output, errors) <- runDjex arguments
  assertEqual "usage failure exit" (ExitFailure 2) exitCode
  assertEqual "usage failure stdout" "" output
  assertContains "usage diagnostic" expected errors
  assertContains "usage hint" "Try 'djex --help'" errors
  assertNoCallStack errors

runDjex :: [String] -> IO (ExitCode, String, String)
runDjex arguments = runDjexInput arguments ""

runDjexInput :: [String] -> String -> IO (ExitCode, String, String)
runDjexInput = readProcessWithExitCode "djex"

runRepl
  :: FilePath
  -> [String]
  -> IO (ExitCode, String, String)
runRepl directory inputs = runDjexInput
  ["repl", "--environment", directory]
  $ unlines $ ":set prompt \"\"" : inputs ++ [":quit"]

withTemporaryEnvironment
  :: [(FilePath, String)]
  -> (FilePath -> IO result)
  -> IO result
withTemporaryEnvironment files action = bracket create removePathForcibly use
 where
  create = do
    temporaryRoot <- getTemporaryDirectory
    (path, handle) <- openTempFile temporaryRoot "djex-environment"
    hClose handle
    removeFile path
    createDirectory path
    pure path
  use path = do
    forM_ files $ \(name, contents) -> do
      let file = path </> name
      createDirectoryIfMissing True $ parentDirectory file
      writeFile file contents
    action path

-- One nested source tree exercises each admission spelling without making the
-- REPL's startup directory accidentally load the fixtures under test.
withReplModuleFixture :: (FilePath -> IO result) -> IO result
withReplModuleFixture = withTemporaryEnvironment
  [ ("empty/.keep", "")
  , ("named/App.hs", unlines
      [ "module App (AppType, appValue) where"
      , "import Lib.Dep"
      , "data AppType = AppConstructor"
      , "appValue :: AppType"
      ])
  , ("named/Lib/Dep.hs", unlines
      [ "module Lib.Dep (DepType, depValue) where"
      , "data DepType = DepConstructor"
      , "depValue :: DepType"
      ])
  , ("directory/Dir/First.hs", unlines
      [ "module Dir.First (FirstType, firstValue) where"
      , "data FirstType = FirstConstructor"
      , "firstValue :: FirstType"
      ])
  , ("directory/Dir/Second.hs", unlines
      [ "module Dir.Second (SecondType, secondValue) where"
      , "import Dir.First"
      , "data SecondType = SecondConstructor"
      , "secondValue :: SecondType"
      ])
  , ("scope/Alpha.hs", unlines
      [ "module Alpha (AlphaType, alphaValue) where"
      , "data AlphaType = AlphaConstructor"
      , "alphaValue :: AlphaType"
      , "alphaHidden :: AlphaType"
      ])
  , ("scope/Beta.hs", unlines
      [ "module Beta (BetaType, betaValue, OtherType, otherValue) where"
      , "data BetaType = BetaConstructor"
      , "betaValue :: BetaType"
      , "data OtherType = OtherConstructor"
      , "otherValue :: OtherType"
      ])
  , ("scope/Surface.hs", unlines
      [ "module Surface (PublicType, publicValue, HiddenType) where"
      , "data PublicType = PublicConstructor"
      , "publicValue :: PublicType"
      , "data HiddenType = HiddenConstructor"
      , "hiddenValue :: HiddenType"
      ])
  , ("scope/AliasLeft.hs", unlines
      [ "module AliasLeft"
      , "  (LeftType, leftValue, Clash, leftClash) where"
      , "data LeftType = LeftConstructor"
      , "leftValue :: LeftType"
      , "data Clash = LeftClashConstructor"
      , "leftClash :: Clash"
      ])
  , ("scope/AliasRight.hs", unlines
      [ "module AliasRight"
      , "  (RightType, rightValue, Clash, rightClash) where"
      , "data RightType = RightConstructor"
      , "rightValue :: RightType"
      , "data Clash = RightClashConstructor"
      , "rightClash :: Clash"
      ])
  , ("scope/RecordSurface.hs", unlines
      [ "module RecordSurface (Record(field), Field) where"
      , "data Field = FieldConstructor"
      , "data Record = RecordConstructor { field :: Field }"
      ])
  , ("type/TypeSurface.hs", unlines
      [ "module TypeSurface"
      , "  (Pair(..), Wrapped(..), Ground(..), Chain(..), Convert(..), Num(..), higher, identity, (<.>)) where"
      , "data Pair a b = Pair a b"
      , "data Wrapped a = Wrapped a"
      , "data Ground = Ground"
      , "data Chain a = End | a :*: Chain a"
      , "class Convert a where"
      , "  convert :: a -> Wrapped a"
      , "class Num a where"
      , "  number :: a"
      , "identity :: a -> a"
      , "(<.>) :: (a -> b) -> a -> b"
      , "higher :: (forall a. a -> a) -> b -> b"
      ])
  , ("type/Prelude.hs", unlines
      [ "module Prelude where"
      , "import Data.Eq (Eq)"
      , "import Text.Show (Show)"
      , "data Integer = Integer"
      , "data Double = Double"
      , "class Num a"
      , "class Num a => Fractional a"
      , "class Enum a"
      , "instance Num Integer"
      , "instance Num Double"
      , "instance Fractional Double"
      , "instance Enum Integer"
      , "instance Enum Double"
      , "instance Data.Eq.Eq Integer"
      , "instance Data.Eq.Eq Double"
      , "instance Text.Show.Show Double"
      ])
  , ("type/Data/Eq.hs", unlines
      [ "{-# LANGUAGE NoImplicitPrelude #-}"
      , "module Data.Eq where"
      , "class Eq a"
      ])
  , ("type/Text/Show.hs", unlines
      [ "{-# LANGUAGE NoImplicitPrelude #-}"
      , "module Text.Show where"
      , "data Text = Text"
      , "class Show a where"
      , "  show :: a -> Text"
      ])
  , ("unresolved/UnresolvedList.hs", unlines
      [ "module UnresolvedList (Local, localValue) where"
      , "import External (T)"
      , "data Local = LocalConstructor"
      , "localValue :: Local"
      ])
  , ("abstract-record/A.hs", unlines
      [ "module A (T, Field, field) where"
      , "data Field = FieldConstructor"
      , "data T = MkT { field :: Field }"
      ])
  , ("reexport/Origin.hs", unlines
      [ "module Origin (Item, itemValue) where"
      , "data Item = ItemConstructor"
      , "itemValue :: Item"
      ])
  , ("reexport/Reexport.hs", unlines
      [ "module Reexport (module Origin) where"
      , "import Origin"
      ])
  , ("package-import/LocalM.hs", unlines
      [ "module LocalM (LocalType, localValue) where"
      , "data LocalType = LocalConstructor"
      , "localValue :: LocalType"
      ])
  , ("package-import/PackageUser.hs", unlines
      [ "{-# LANGUAGE PackageImports #-}"
      , "module PackageUser where"
      , "import \"example-package\" LocalM"
      ])
  , ("export-scope/CollisionLeft.hs", unlines
      [ "module CollisionLeft (clash) where"
      , "clash :: a -> a"
      ])
  , ("export-scope/CollisionRight.hs", unlines
      [ "module CollisionRight (clash) where"
      , "clash :: a -> a"
      ])
  , ("export-scope/Collision.hs", unlines
      [ "module Collision (module X) where"
      , "import CollisionLeft as X"
      , "import CollisionRight as X"
      ])
  , ("export-scope/Duplicate.hs", unlines
      [ "module Duplicate (module X) where"
      , "import CollisionLeft as X"
      , "import CollisionLeft as X"
      ])
  , ("export-scope/TypeSide.hs", unlines
      [ "module TypeSide (Same) where"
      , "data Same = TypeConstructor"
      ])
  , ("export-scope/ValueSide.hs", unlines
      [ "module ValueSide where"
      , "data Other = Same"
      ])
  , ("export-scope/Namespace.hs", unlines
      [ "module Namespace (module X) where"
      , "import TypeSide as X"
      , "import ValueSide as X"
      ])
  , ("symlink/Real.hs", unlines
      [ "module Real (RealType, realValue) where"
      , "data RealType = RealConstructor"
      , "realValue :: RealType"
      ])
  , ("symlink/UsesAlias.hs", unlines
      [ "module UsesAlias where"
      , "import Alias"
      ])
  , ("rename/Target.hs", unlines
      [ "module Before (BeforeType, beforeValue) where"
      , "data BeforeType = BeforeConstructor"
      , "beforeValue :: BeforeType"
      ])
  , ("rename/After.source", unlines
      [ "module After (AfterType, afterValue) where"
      , "data AfterType = AfterConstructor"
      , "afterValue :: AfterType"
      ])
  , ("named-alias/A.hs", unlines
      [ "module A (AType, aValue) where"
      , "data AType = AConstructor"
      , "aValue :: AType"
      ])
  , ("directory-links/Inside.hs", unlines
      [ "module Inside (InsideType, insideValue) where"
      , "data InsideType = InsideConstructor"
      , "insideValue :: InsideType"
      ])
  , ("linked-file/Linked.hs", unlines
      [ "module Linked (LinkedType, linkedValue) where"
      , "import OutsideSibling"
      , "data LinkedType = LinkedConstructor"
      , "linkedValue :: LinkedType"
      ])
  , ("linked-file/OutsideSibling.hs", unlines
      [ "module OutsideSibling (OutsideType, outsideValue) where"
      , "data OutsideType = OutsideConstructor"
      , "outsideValue :: OutsideType"
      ])
  , ("escaped-directory/Escaped.hs", unlines
      [ "module Escaped (EscapedType, escapedValue) where"
      , "data EscapedType = EscapedConstructor"
      , "escapedValue :: EscapedType"
      ])
  , ("bundle/EmptyData.hs", unlines
      [ "{-# LANGUAGE EmptyDataDecls #-}"
      , "module EmptyData (E(..)) where"
      , "data E"
      ])
  , ("bundle/InvalidSynonym.hs", unlines
      [ "module InvalidSynonym (S(..)) where"
      , "data Field = Field"
      , "type S = Field"
      ])
  ]

withMissingPath :: (FilePath -> IO result) -> IO result
withMissingPath action = do
  temporaryRoot <- getTemporaryDirectory
  (path, handle) <- openTempFile temporaryRoot "missing-djex-environment"
  hClose handle
  removeFile path
  action path

-- The CLI suite intentionally has no filepath dependency. Its subprocess and
-- shell coverage is already POSIX-specific, so a tiny local join keeps nested
-- temporary fixtures readable without widening the test component's API.
(</>) :: FilePath -> FilePath -> FilePath
directory </> entry = directory ++ "/" ++ entry

infixr 5 </>

parentDirectory :: FilePath -> FilePath
parentDirectory path = case break (== '/') $ reverse path of
  (_, []) -> "."
  (_, _ : reversedParent) -> reverse reversedParent

assertNoCallStack :: String -> Assertion
assertNoCallStack output = assertBool "controlled failure exposed a CallStack" $
  not $ "CallStack" `isInfixOf` output
