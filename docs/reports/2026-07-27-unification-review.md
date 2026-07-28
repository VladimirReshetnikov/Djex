# Djex unification review

Date: 2026-07-27

## Scope and method

This pass reviewed `djinn/`, `exference/`, and the shared `src/` and
`synthesis/` layers as one library, with particular attention to duplicated
ownership, immutable-session construction, diagnostics, and the GHCi-shaped
shared REPL. Historical frontends and exposed compatibility modules were also
checked where their behavior remains part of the package contract.

The review consolidated code where one neutral owner or one sealed derived
view removed real duplication. It did not merge backend algorithms merely
because they solve related problems: Djinn's proof search and Exference's
ranked exploration retain different guarantees, internal state, and policy.
Nine focused changes resolved the findings below.

## Correctness and usability findings resolved

1. **Shared command lexing depended on a Djinn-internal module.** Haskell
   line-comment stripping used by the merged REPL now belongs to neutral
   `Language.Haskell.Djex.Text`. The historical
   `Djinn.Internal.HIdentifier` export delegates to that owner, preserving its
   compatibility API without making shared code depend on a backend-internal
   module. Quoted strings, character literals, and symbolic operators retain
   the established lexical behavior.
2. **Djinn scope repair conflated type and value namespaces.** A value
   constructor whose spelling matched a referenced external type could
   incorrectly satisfy the reference, leaving the projected Djinn inventory
   invalid. Projection now tracks type-constructor and class requirements
   separately from value ownership and synthesizes the required abstract type
   stub. The same REPL pass also fixed implicit `:edit` so a starred module
   opens its canonical source file rather than passing display-only `*` text
   to the editor. Bare `import`, `import qualified`, and `import safe` now
   complete loaded modules, with exact lowercase keyword recognition matching
   the parser.
3. **The historical Exference command hid actionable warnings by default.**
   Recoverable loader and session warnings, including malformed rating files
   and omitted recursive capabilities, now appear once on stderr without
   `--verbose`. Informational progress remains quiet by default and retains
   its historical verbose stdout presentation.
4. **Every `:type` query rebuilt an environment-wide inspection view.** The
   Exference session now derives and materializes the complete term-scheme map
   and inspection class environment once at its immutable sealing boundary.
   `:type` consumes those cached, parser-neutral views instead of repeatedly
   preparing and lowering the full inventory. Inspection deliberately remains
   complete even when synthesis policy or prompt scope narrows the searchable
   dictionary.
5. **`:info` did not follow constructor and method ownership into instances.**
   A query for a data constructor now resolves its owning datatype, and a
   query for a class method resolves its owning class, before related instances
   are collected. Type, constructor, class, and method spellings therefore
   report the same participating instances without duplicate lines.
6. **Djinn verbose help printed a literal package-version placeholder.** The
   compatibility REPL now obtains the Cabal-generated Djex version and renders
   the same welcome example as the live startup banner. The regression derives
   its expectation from package metadata rather than hard-coding a release.
7. **Interactive state publication was not atomic across backends after an
   internal Djinn projection failure.** A successful Exference replacement or
   scope edit was published before Djinn projection was known to succeed, so
   the same prompt could expose the new source scope to Exference and Djinn's
   unrelated standard environment. Projection refresh now returns a checked
   candidate. Interactive loads, scope edits, fix-policy rebuilds, and Djinn
   axiom-policy changes publish only after both sessions are ready; otherwise
   they report the diagnostic and retain the complete preceding state. Initial
   startup alone may use the standard Djinn recovery because it has no earlier
   source projection to preserve.
8. **Real-GHC evaluation silently weakened `import safe`.** The synthesis
   scope accepts Haskell's safe-import syntax, but Hint's structured import
   context has no field for that flag. Evaluation used to translate the entry
   as an ordinary import. It now validates the complete context before loading
   workspace modules, refuses the downgrade, and uses the existing
   all-or-nothing Prelude fallback with an advisory. The synthesis scope itself
   remains unchanged.
9. **A fatal legacy Exference load discarded earlier warnings.** The loader
   report deliberately retains diagnostics accumulated before final inventory
   validation, but the compatibility command inspected the fatal result before
   emitting them. It now emits the complete diagnostic stream first, so a
   malformed rating warning remains visible beside a later kind or inventory
   failure, in the same order as the merged command.

## Consolidation outcome

- Shared lexical policy has a neutral owner while the historical export is a
  compatibility re-export.
- Read-only REPL inspection data is derived once beside the authoritative
  immutable session rather than reconstructed by an individual command.
- Module completion, source editing, `:info`, and Djinn projection now consume
  the same loaded-workspace and namespace facts used by command execution.
- Recoverable diagnostics follow one user-visible severity rule across the
  merged command and the Exference compatibility command.
- Workspace, scope, and projection publication is one cross-backend
  transaction; success is reported only after both checked sessions exist.
- Evaluation refuses syntax its interpreter context cannot preserve, and
  compatibility diagnostics retain their production order through failures.
- Focused subprocess and unit regressions cover each corrected behavior at the
  boundary where users observe it.

## Deliberate compatibility boundaries retained

- The `djex` REPL is the unified interactive frontend, but the historical
  `djinn` REPL remains available with its declaration editor and legacy
  grammar; the historical `exference` executable remains a one-shot command.
  Replacing either compatibility contract with the shared command grammar
  would be a migration, not code deduplication.
- The exposed `Djinn`, `Djinn.Core`, and `Language.Haskell.Exference.*`
  research surfaces remain importable. New integrations should use the checked
  `Language.Haskell.Djex` facades, but removing or silently reshaping the old
  modules requires an explicit compatibility release.
- Djinn remains a terminating proof search for its supported logic and can
  carry proof-backed non-inhabitation evidence. Exference remains a ranked,
  resource-bounded heuristic search whose exhaustion is not such a proof.
  Their search cores, evidence, class policies, and caches should not be forced
  into a common implementation record.
- Djinn's stored canonical binder spellings and Exference's numeric identities
  with render-time hints remain observable contracts. The shared `Candidate`
  constructor also remains public for compatibility, so stable renderers must
  continue to validate caller-constructed values rather than trusting their
  origin.
- `:type` and `:kind` are structural inspection commands and bare input is a
  synthesis type. Only explicit `:eval` invokes real GHC. Making Djex look more
  like GHCi must not blur that execution boundary.
- The `djinn/`, `exference/`, `synthesis/`, and root source directories remain
  useful provenance and dependency boundaries even though Cabal compiles them
  into one library. Flattening the filesystem would not itself remove a
  representation or an invariant.

## Sensible future work

The remaining opportunities are larger API or ownership projects and should
not be performed as mechanical renames:

- Make `Candidate` construction opaque in a future compatibility-breaking
  release and consider replacing caller-owned `String` hints with an opaque,
  bounded representation. That would move ordinary validation to entry and
  permit a fresh decision about whether Exference's narrow pure-rendering
  defense is still required for exceptional caller values.
- Introduce a stable neutral inspection service before sharing more of
  `:type` and `:kind`. Their current private implementations correctly reuse
  the Exference session's prepared inventory, but a public service should own
  term schemes, class entailment, synonym normalization, and scope conversion
  without exposing backend internals.
- Revisit the Haskell-source frontend's `SourceEnvironment`, class
  environment, search dictionary, and neutral inventory projections only with
  an explicit invariant map. They serve different phases today; collapsing
  records without moving authority would hide rather than remove complexity.
- Consider unifying private `ReplScope` with public `ExferenceQueryScope` in a
  future API migration. The former owns validated prompt state while the
  latter is a compatibility-shaped parser projection, so replacing either
  requires a constructor and error-contract plan.
- Add a narrow pure command/driver test seam if future REPL work outgrows
  subprocess coverage. Exposing the current private Haskeline workers solely
  for tests would weaken the documented API tiers and is not warranted now.
- Raise Haddock coverage for the historical raw Djinn and Exference surfaces
  as a separate compatibility project. The curated Djex facades are fully
  documented and library documentation generation succeeds, but inherited
  research modules still report missing comments and links to hidden
  representation names.

## Validation

The focused slices completed during the nine changes include:

- all 70 Djinn unit tests plus the shared line-comment regression;
- the strict warning-clean library build and all 67 merged CLI tests for the
  namespace, editing, completion, cached-inspection, and safe-evaluation
  changes;
- all 25 Exference CLI tests for complete quiet/default diagnostics; and
- all 419 Exference unit tests for the sealed inspection caches.

Constructor/method `:info` ownership and Cabal-derived Djinn help text have
dedicated subprocess regressions. The final whole-tree gate is deliberately
serial because several CLI suites execute freshly linked tools:

```console
cabal build all --ghc-options='-Werror -Widentities -Wincomplete-patterns -Wincomplete-record-selectors'
cabal check
cabal test all -j1 --test-show-details=direct
cabal haddock lib:djex --haddock-hyperlink-source
cabal sdist
```

All five commands completed successfully on GHC 9.12.4. The serial test run
passed all 12 suites and 961 tests; `cabal check` was clean, and the source
tarball contains this report and the updated guides. Haddock generated the
complete library site while retaining the legacy coverage/link warnings noted
as future work above.
