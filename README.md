# Djex

Djex is the codename for the synthesis tool being formed by merging Djinn and
Exference. The name contracts **Dj**inn and Exference's **ex**. It is one Cabal
package: both independently testable engines and their neutral foundation now
share a version, source distribution, data-file layout, and build graph while
their duplicated frontend, validation, environment, search-envelope, and
generated-output infrastructure is progressively consolidated.

## Components

- The default `djex` library exposes `Language.Haskell.Djex`, whose backend
  identities and conservative capability metadata provide the neutral entry
  point. `Language.Haskell.Djex.Djinn` and
  `Language.Haskell.Djex.Exference` run both engines through the shared
  query/evidence/search envelope. The default library re-exports the stable
  synthesis vocabulary and checked adapter APIs needed by ordinary clients;
  historical and research-oriented backend modules remain available only from
  their explicit named sublibraries.
- `synthesis/` supplies the public named `synthesis` sublibrary: validated
  names, types, kinds, declarations, environments, diagnostics, generated
  output, and operational search status.
- `djinn/` supplies the public `djinn-core` proof-search sublibrary and the
  `djinn-frontend` compatibility/REPL sublibrary.
- `exference/` supplies the public parser-independent `exference-core`
  sublibrary and the `exference-frontend` Haskell-source/environment-loader
  sublibrary, which also owns the checked Djex/Exference session adapter.

The historical `djinn` and `exference` executable names remain available. The
package also retains `djex-tests`, `synthesis-tests`, all three Djinn test suites,
`exference-tests`, `exference-cli-tests`, and the `djinn-bench` benchmark. This
preserves differential testing between the mature engines until their common
frontend can replace the two compatibility surfaces.

## Dependency migration

The single-package layout intentionally replaces the three former package
identities. Existing Cabal dependencies migrate as follows:

| Former dependency | Djex dependency |
| --- | --- |
| `haskell-synthesis` | `djex:synthesis` |
| `djinn:djinn-core` | `djex:djinn-core` |
| unnamed `djinn` library | `djex:djinn-frontend` |
| `exference:exference-core` | `djex:exference-core` |
| unnamed `exference` library | `djex:exference-frontend` |

New clients can depend on the unnamed `djex` library for the checked common
facade and synthesis vocabulary. Compatibility or low-level clients opt into
`djex:djinn-core`, `djex:djinn-frontend`, `djex:exference-core`, or
`djex:exference-frontend` explicitly instead of acquiring those research APIs
transitively. Build-tool dependencies for the historical commands remain
`djex:djinn` and `djex:exference`; their executable names are unchanged.

The filesystem and Cabal-project migration is equally deliberate:

- the former top-level `synthesis/`, `djinn/`, and `exference/` trees now live
  at `djex/synthesis/`, `djex/djinn/`, and `djex/exference/`;
- their separate package descriptions and project files have been replaced by
  `djex/djex.cabal`; the repository-root `cabal.project` is the single solver
  root, and Cabal discovers it by walking to the parent when invoked here;
- package-generated code must import `Paths_djex` instead of `Paths_djinn` or
  `Paths_exference`; version discovery and installed-data lookup now belong to
  Djex as a whole; and
- Exference's installed environment is a Djex data directory. Use
  `getDataFileName "exference/environment"` from `Paths_djex`, rather than
  assuming either a checkout-relative path or the old package data root.

The root Cabal project enables tests and benchmarks, so `cabal build all` and
`cabal test all` exercise the same component graph whether invoked at the
repository root or in `djex/`, with one solver plan and build cache.

## Building

The repository root contains the canonical Cabal project; Cabal discovers that
parent project when commands start in this directory. From either location,
build and test the complete graph:

```console
cabal build all
cabal test all --test-show-details=direct
```

Useful component and compatibility-executable targets include:

```console
cabal build djex:lib:djex djex:lib:synthesis
cabal build djex:lib:djinn-core djex:lib:djinn-frontend
cabal build djex:lib:exference-core djex:lib:exference-frontend
cabal run djinn
cabal run exference -- --first "a -> a"
cabal bench djinn-bench
```

The backend subdirectories are source roots, not independent Cabal projects;
run package commands from the repository root or from `djex/`.

## Query boundary

Exference core names are an opaque, validated subset of the shared synthesis
name domain. The compatibility import `QualifiedName(..)` still bundles the
four historical pattern views, but the input-bearing `QualifiedName` and
`TupleCon` patterns are intentionally match-only: construct ordinary names and
boxed tuples with `mkQualifiedName` and `mkBoxedTupleName`, which can report
invalid spelling, qualification, or tuple arity. `ListCon` and `Cons` remain
total constants. Representation reflection through `Data` or `Generic` is no
longer part of this compatibility surface, so it cannot manufacture names
outside Exference's supported subset. Exference constraints likewise store a
strict narrowed nominal name and their argument list directly; complete checked
conversion to and from `Language.Haskell.Synthesis.Constraint` happens at the
shared boundary rather than during ordinary field access.

`Language.Haskell.Synthesis.Query` shares the target, goal, contexts, logical
evidence, and search-batch shape without pretending that both engines accept
the same types or options. `mkDjinnSession` seals an opaque Djinn environment;
`runDjinnQuery` returns shared candidates containing structured generated
clauses, empty residual constraints, and Djinn's unused-binder ranking details
in one terminal batch. A proof beyond `optionCutoff` produces
`Truncated CandidateLimitReached` without forcing the proof-stream suffix.
Proof-backed `ProvedUninhabitable`, target-reference evidence, and
budget-limited `NoEvidence` remain distinct from the batch's operational
`Finished` or `Truncated` completion.

`mkExferenceSession` similarly computes the backend-supported projection and
seals its search environment once. The source boundary tags class methods by
their qualified owner, nests them under the common class declaration for
validation, and lowers each rated selector exactly once into Exference's flat
search inventory without changing source order. Unsupported rank-N
introduction and elimination capabilities remain visible as structured
omissions and warning diagnostics instead of disappearing per query.
The Haskell-source loader is likewise fail-closed at its vocabulary boundary:
after parsing, but before constructing any partial inventory, it reports
source-ordered `UnsupportedVocabularyOccurrence` values for type/data families,
GADTs, derived or overlapping instances, functional dependencies, associated
families and defaults, declaration splices, role annotations, and XML hybrid
modules. Each occurrence carries the stable
`EXF_UNSUPPORTED_VOCABULARY` diagnostic code and its exact source span.
Imports, fixities, ordinary value and method bodies, pattern vocabulary,
default declarations, and operational pragmas remain accepted because they do
not change the nominal type/class inventory. These forms are explicit current
limitations rather than syntax that can silently disappear during loading.
`parseExferenceRequest`
elaborates a Haskell type against the session's retained names, synonyms,
classes, and kind assumptions; `runExferenceQuery` validates only the varying
goal and search policy, then returns a lazy sequence of shared result batches.
Those batches preserve queue/depth pruning, nominal binding usage, residual
constraints, statistics, and rendering hints without forcing the remaining
trace. Each generated expression is wrapped in a target-bearing shared
`FunctionClause`. Candidate selection and rendering remain presentation
policies outside both session operations. The shared `Selection` module now
provides first, global-best, streaming-all, batch-lookahead, and preferred-tier
lookahead policies over either backend's result envelope. `TypeRender` prints
shared types and constraints from tagged variable-name hints without collapsing
flexible and rigid identities.

The `exference` compatibility executable is a thin consumer of this boundary:
it loads and seals one session, parses every requested type through
`parseExferenceRequest`, selects shared candidates, and renders their generated
expression bodies. Exact nominal session policy replaces its former
occurrence-text filtering of recursion helpers. Parse, kind, option, and search
failures are structured diagnostics on stderr with failure exit status;
repeated inputs are all processed and conflicting presentation modes are
rejected. Its historical ranking vector remains an explicit compatibility
profile. `--short` now adds backend-neutral structural expression size to the
candidate cost instead of being a dead option.

The `djinn` compatibility frontend likewise retains its declaration REPL while
storing each editable `Environment` together with the exact sealed
`DjinnSession`. Successful mutations replace that pair transactionally; type
queries and instance methods consume shared evidence, progress, metadata, and
`FunctionClause` output through `runDjinnQuery`. Startup-file mode now carries
aggregate failure status across later commands and `:clear`, accepts settings
on either side of filenames, and rejects unknown or ambiguous option prefixes;
interactive recovery remains unchanged.
