# Djex source-semantics follow-up

Date: 2026-07-27

## Scope

This follow-up reviewed the shared Haskell-source boundary and unified REPL
after the broader [unification review](2026-07-27-unification-review.md). It
traced source syntax from parsing through module discovery, import/export
resolution, neutral inventory construction, both backend projections, and
interactive inspection. It also revisited the historical frontends where they
still share checked session construction with the merged library.

The aim was semantic agreement, not merely similar naming. A source spelling
must retain the same module identity, namespace, ordering, and visibility at
every layer that can represent it. When the neutral model cannot preserve a
Haskell distinction, loading now fails explicitly instead of silently choosing
an approximation.

## Findings resolved

1. **Infix class constraints were not normalized at the shared source
   boundary.** Prefix and symbolic constraint heads now enter the same ordered
   application spine in signatures, superclass declarations, and instance
   prerequisites. Promoted operators remain distinct from class operators.
2. **Package-qualified imports could lose their package identity.** The neutral
   name model stores a module and occurrence but no package key, so accepting
   this syntax could bind an unrelated same-named module. Every source loader
   now rejects it during unsupported-vocabulary preflight, including when the
   import is unused.
3. **Implicit Prelude switches ignored source order.** `LANGUAGE` pragmas,
   parser-mode flags, and both `-fimplicit-prelude` spellings now update one
   ordered state machine. Later `ImplicitPrelude`, `NoImplicitPrelude`,
   `RebindableSyntax`, and `NoRebindableSyntax` choices can therefore restore
   or suppress the implicit import as GHC does.
4. **Ordinary import cycles had an unstable cutoff.** Filesystem workspaces and
   in-memory source closures now use one stable dependency traversal and reject
   the first non-`SOURCE` cycle with a located diagnostic. A `SOURCE` edge still
   acts as the explicit boot-interface break. Dependency discovery itself uses
   a queue and indexed module maps rather than repeated list indexing and
   append operations.
5. **External import routes were collapsed too early.** Each import route now
   retains its own alias, exact/open surface, and hiding policy. An exact loaded
   alias cannot close an independent open external route, and a hiding list on
   an unloaded module remains enforceable during elaboration.
6. **Discarded type-operator fixities could change meaning silently.** Explicit
   parentheses remain supported, while an unparenthesized operator chain whose
   grouping depends on unavailable fixity declarations is rejected before it
   reaches the neutral inventory.
7. **The prompt scope flattened Haskell's type and value namespaces.** Import
   lists, hiding clauses, aliases, exports, and re-exports now carry a
   route-local namespace surface. Type queries consume the type surface;
   expression inspection and backend search consume the value surface. Djinn
   receives both projections separately, so hiding a constructor makes the
   datatype abstract instead of leaking the constructor back into scope.
   Completion likewise stops suggesting value-only names in synthesis and
   kind positions while retaining both namespaces where the grammar permits
   them.
8. **Startup loading had partial and misleading failure paths.** Path lookup,
   inspection, canonicalization, strict reads, and whole-script parsing now
   produce structured diagnostics. Missing startup files remain silent, while
   a failed candidate is never announced as loaded. The historical Djinn
   frontend likewise treats checked standard-session construction as fallible;
   a failed `:clear` keeps the last usable session.
9. **Djinn `:info` bypassed the shared prompt resolver.** Source-backed
   inspection now resolves bare, canonical-qualified, and aliased spellings to
   one source identity before translating to the sealed Djinn session's prompt
   spelling. Exference and `both` mode use the same resolution decision.
10. **Several helper paths encoded impossible list states or quadratic
    reconstruction.** Declaration-head splitting now uses a linear reverse
    accumulator, diagnostic collection uses its nonempty invariant directly,
    and dependency discovery maintains stable queues and maps.

## Deliberate boundaries

- Djinn proof search and Exference ranked heuristic search retain different
  engines, evidence, limits, and omission policies. Uniform request/result and
  source boundaries do not make their conclusions interchangeable.
- Package identity and arbitrary user type-operator fixity are deliberately
  outside the current neutral source model. The loaders fail closed at those
  boundaries rather than extending core `Name` identity or embedding a Haskell
  fixity environment incidentally.
- `SOURCE` imports preserve dependency-order semantics but Djex does not parse
  or reconcile `.hs-boot` declarations. The ordinary source module still owns
  the loaded inventory.
- The historical `djinn` and `exference` commands keep their compatibility
  grammars. The `djex` command is the single GHCi-shaped, cross-backend REPL.
- Source elaboration and interactive prompt scope still have separate policy
  engines because they produce different artifacts: the former resolves types
  into a neutral inventory, while the latter models a mutable Haskell prompt.
  Their tests now exercise the same import, Prelude, namespace, and dependency
  cases. Extracting a common declarative import-policy kernel is reasonable
  future work; merging the state machines directly would obscure their
  different failure and publication contracts.

## Validation contract

Correctness changes have focused regressions in the library or CLI suites;
helper and traversal simplifications remain covered by the existing workspace
and source-loading suites. The final handoff runs the complete graph serially
because several integration suites execute freshly linked tools:

```console
cabal build all --ghc-options='-Werror -Widentities -Wincomplete-patterns -Wincomplete-record-selectors'
cabal test all -j1 --test-show-details=direct
cabal check
cabal haddock lib:djex --haddock-hyperlink-source
cabal sdist
```

The build is pinned and tested with GHC 9.12.4. Exact suite counts and command
results are recorded in the draft pull request after the release gate.
