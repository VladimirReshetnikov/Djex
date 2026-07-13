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
  query/evidence/search envelope. The default library also re-exports the
  stable compatibility and synthesis APIs so a consumer can migrate while
  depending on one library.
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

New clients can depend on the unnamed `djex` library for the common facade and
all stable, non-internal backend modules. Build-tool dependencies become
`djex:djinn` and `djex:exference`; the executable names themselves are unchanged.
Backend-specific clients may depend directly on `djex:djinn-core` or
`djex:exference-frontend` without linking the other engine.

## Building

The repository root and this directory each contain a Cabal project for the
same single package. From either location, build and test the complete graph:

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

`Language.Haskell.Synthesis.Query` shares the target, goal, contexts, logical
evidence, and search-batch shape without pretending that both engines accept
the same types or options. `mkDjinnSession` seals an opaque Djinn environment;
`runDjinnQuery` returns structured generated clauses in one terminal batch.
Proof-backed `ProvedUninhabitable`, target-reference evidence, and
budget-limited `NoEvidence` remain distinct from the batch's operational
`Finished` or `Truncated` completion.

`mkExferenceSession` similarly seals a checked source environment and computes
its backend-supported projection once. Unsupported rank-N introduction and
elimination capabilities remain visible as structured omissions and warning
diagnostics instead of disappearing per query. `parseExferenceRequest`
elaborates a Haskell type against the session's retained names, synonyms,
classes, and kind assumptions; `runExferenceQuery` validates the finite input
eagerly and then returns a lazy sequence of shared result batches. Those
batches preserve queue/depth pruning, nominal binding usage, residual
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
