# Djex final convergence review

Date: 2026-07-17

## Scope and method

This pass started from a hard reset to `origin/main` and reviewed the merged
`djinn/` and `exference/` trees as one library. It covered the checked adapters,
their historical frontends, both search/checking cores, shared query and
generated-code boundaries, package metadata, public Haddock, and the current
guides. Findings were reproduced before implementation and landed as focused
commits with regression coverage.

The goal was architectural convergence, not an artificial merger of two search
algorithms with different guarantees. Uniformity now means that shared source
facts, request/result envelopes, invariant ownership, diagnostics, and
rendering follow the same shape; proof search and heuristic search retain their
own semantics.

## Correctness findings resolved

1. **Djinn accepted global names in the wrong lexical role.** The legacy and
   direct proof-lowering paths could admit a lowercase constructor or an
   uppercase free value. One role-aware validator now serves both paths and
   rejects the malformed generated tree before it reaches rendering.
2. **Exference's independent checker trusted known class arities.** Search
   sealing rejected malformed known constraints, but the public checker could
   certify expressions whose goal, residual, assumption, or binding constraint
   applied a known class to the wrong number of arguments. The checker now
   validates every reachable constraint against the class environment while
   deliberately retaining support for unknown external obligations.
3. **Empty datatypes had no Exference elimination rule.** Checked sessions
   accepted nonrecursive empty datatypes, yet search silently ignored their
   eliminators. Search now emits a structured empty case, charges exactly one
   scrutinee use, and creates no unreachable branch goals. The independent
   checker separately verifies that an empty case's scrutinee unifies with a
   declared empty deconstructor and backtracks from an unchanged state.
4. **The checker and live search disagreed about deconstructor validity.** The
   raw checker validated the component types but could accept a headless or
   function-headed deconstructor and thereby certify a pattern match that
   cannot denote Haskell. Both entrances now use one typed structural
   validator for nominal heads and bound field variables while preserving the
   live search error vocabulary and existing validation precedence.
5. **Synonym diagnostics could be reordered by phase.** Haskell-source
   extraction accumulated raw conversion failures before semantic synonym
   failures, even when the semantic failure appeared earlier in source. The
   loader now projects all results back through their original declaration
   slots, preserving deterministic source order across phases.
6. **The legacy Exference command leaked loader constructors.** Fatal loading
   errors used `Show EnvironmentLoadError`, discarding the stable compiler-like
   presentation already exposed by the library. The command now renders every
   projected diagnostic with its code, path, declaration span, and context,
   keeping the historical prefix and exit behavior.

## Consolidation and simplification

- Djinn now mirrors Exference's invariant ownership. Independent private
  `Internal.Request` and `Internal.Session` modules own the opaque request plan
  and the authoritative prepared environment respectively; they meet only in
  the public facade. Parsing, execution, failure translation, and rendering
  remain in that facade, which fell from 532 to 225 lines without changing its
  public exports or signatures.
- The Djinn compatibility REPL obtains its type, function, and class tables
  from one opaque declaration snapshot. Its `:environment` command no longer
  reconstructs and validates the same raw compatibility environment three
  times.
- Installed Exference environment discovery has one public owner. The checked
  Haskell-source facade now exposes the default path and default session
  loaders; the merged and historical commands delegate to those operations
  instead of repeating generated-path knowledge.
- The checked Exference facade is organized around sessions, requests, and
  results, with the workflow and policy semantics documented at their invariant
  owners. The Djinn curated Haddock surface is complete, and Exference's only
  counted gaps are record-pattern selectors whose arguments are documented on
  the patterns themselves.
- The root architecture and library guides replace migration archaeology as
  the starting point. The synthesis API map was shortened to current ownership
  and import guidance, and backend READMEs were corrected to describe the
  single-package layout and present behavior.

## Deliberate differences retained

- Djinn's LJT engine may prove non-inhabitation; a completed Exference search
  remains a bounded heuristic observation. Their evidence and progress must not
  be collapsed into one interpretation.
- Djinn's canonical binder spellings are part of its stored-tree and candidate
  de-duplication contract. Exference's numeric locals and checked render-time
  hints remain a different, intentional representation.
- Recursive Exference deconstructors remain omitted with a structured warning.
  Adding them requires a sound termination policy, not merely another branch in
  the current eliminator dispatch.
- Exference's bounded evaluation of caller-owned lazy rendering hints remains
  the narrow `unsafePerformIO` trust boundary. It rejects cyclic, overlong, or
  synchronously exceptional hints while preserving asynchronous exceptions;
  removing it would make a pure compatibility renderer partial on adversarial
  lazy strings.
- The checked source-session bridge continues to call the private sealer
  directly. Its small overlap with the historical session facade preserves the
  dependency-inward architecture and is not duplicated business logic.

## Validation contract

Focused regressions cover every finding above, including high-volume Djinn
lowering properties, raw Exference checker inputs, search/checker agreement,
source locations and order, empty-case rendering, and command subprocesses.
The release handoff additionally runs:

```console
cabal build all
cabal test all -j1 --test-show-details=direct
cabal check
cabal haddock lib:djex --haddock-hyperlink-source
cabal sdist
```

The serial test run is intentional: one earlier parallel Cabal invocation hit
a transient executable-text-busy race while replacing a test binary; the
affected suite passed immediately when rerun and no test failure was involved.
