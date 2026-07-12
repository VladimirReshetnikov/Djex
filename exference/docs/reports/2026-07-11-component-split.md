# Exference core/frontend component split

- Date: 2026-07-11
- Toolchain: GHC 9.12.4, Cabal 3.16.1.0
- Integration target: `djinn-core`

## Finding

The 2026-07-10 review identified the package boundary as the next integration
blocker. Although the source called part of itself “Core”, all 31 production
modules belonged to one Cabal library. A consumer of the search engine
therefore inherited `haskell-src-exts`, parsing, filesystem, and process
dependencies. More subtly, assigning the modules to two Cabal components while
leaving both under one source root caused GHC to rediscover and compile the core
as frontend home modules; stanza-level separation alone was not a real
dependency boundary.

## Change

The package now has two physically separate library components:

```text
src-core/  -> public named library exference:exference-core (19 source modules)
src/       -> unnamed exference frontend library             (11 source modules,
                                                               plus Paths_exference)
```

The named component is explicitly `visibility: public`; Cabal's default for a
named sublibrary is private, so source separation alone was not enough to make
the documented consumer boundary real.

The core owns only the internal type system, unifier, constraint solver,
expression representation/checker/simplifier, search state, and search engine.
The frontend owns Haskell source conversion, diagnostics, environment loading,
ratings files, and compatibility helpers. The frontend depends on
`exference-core` and re-exports its historical public modules, so existing
clients of the unnamed `exference` library retain their imports.

This is intentionally the same dependency direction as Djinn: a named core
library can be consumed without an executable or source-syntax frontend. It is
also the first enforceable merge boundary. A future common parser and session
API can target both named libraries while the LJT and heuristic search engines
remain independent.

## Dependency reduction

`exference-core` no longer exposes or depends on frontend-only packages:

- `haskell-src-exts`
- `process` and `directory`
- `bifunctors`
- `split`
- `mmorph`
- `safe`
- `deepseq-generics`

The last three were legacy implementation conveniences rather than semantic
requirements: `transformers` supplies `lift`, an explicit lookup replaces the
partial `fromJustNote`, and modern `deepseq` supplies Generic defaults for the
same `NFData` instances. The resulting direct core dependency set is:

```text
base, haskell-synthesis, containers, pretty, deepseq, pqueue,
transformers, mtl, vector, lens, multistate
```

The frontend retains `deepseq` because environment parsing deliberately forces
lazy file contents while still inside its exception boundary. That dependency
was confirmed by a clean forced rebuild rather than inferred from the former
combined stanza.

## Validation

The component boundary was checked with a forced build, which compiled the core
once as 18 modules and the frontend once as 13 modules. The following then
passed:

```text
cabal test all --test-show-details=direct   # 103 library + 5 CLI + 48 shared
cabal check                                 # no errors or warnings
git diff --check
```

The current matrix has grown to 103 deterministic library/frontend regressions,
five CLI subprocess scenarios, and the 48 shared-foundation tests. All Cabal
components import one Haskell2010/`-Wall -Wcompat` policy stanza, and test-only
dependency bounds now match the production packages.
