# Remove compile-time search instrumentation

- Date: 2026-07-11
- Resolves: R-15 of the Exference code review
- Toolchain: GHC 9.12.4, Cabal 3.16.1.0

## Finding

Two manual Cabal flags changed the representation and public output of the
search engine. `linkNodes` inserted a recursive predecessor pointer into every
`SearchNode`; `buildSearchTree` inserted an optional tree into every result and
reconstructed node identities with `StableName` and `unsafePerformIO`. The
instrumentation therefore affected allocation, evaluation, exported record
shape, and purity even though its only consumer was a disabled historical CLI
debug command.

The same CLI advertised serial and parallel modes, but every parallel branch
printed a warning and executed the serial implementation. Production modules
also retained dozens of unused imports behind a global warning suppression.

## Remediation

- Removed `linkNodes` and `buildSearchTree` and all CPP branches.
- Removed predecessor links, stable-name bookkeeping, `unsafePerformIO`, and
  the conditional `chunkSearchTree` field.
- Deleted the unreachable `SearchTree` and `Flags_exference` modules and their
  sole implementation-specific regression.
- Removed the historical CLI's tree, serial, and unimplemented parallel
  options, and removed its placeholder `TODO` help line.
- Deleted the 600-line embedded `MainTest` harness and removed Hood,
  `data-pprint`, and the dead external pointfree helpers. The executable is now
  an ordinary modern-build target rather than an opt-in component.
- Deleted the resulting dead imports and the library-level
  `-fno-warn-unused-imports` suppression.
- Removed `unordered-containers`, `hashable`, `process`, `bifunctors`, and the
  unnecessary direct `template-haskell` dependency from the supported
  libraries. (`Data.Bifunctor` comes from `base`; Template Haskell is used
  through `lens` without importing `template-haskell` modules directly.)

The core now has one production data model regardless of build flags. Future
observability should be an explicit event stream or callback at the backend
envelope, not a compile-time mutation of search values.

The first end-to-end CLI smoke exposed a separate validation defect: shipped
function ratings include negative bonuses, but the validator applied the
non-negative heuristic-penalty policy to them and rejected the entire default
environment. Function ratings now require finiteness but may be signed;
heuristic penalties remain finite and non-negative. A regression covers the
distinction. The CLI also omits unsupported nested-forall environment bindings
instead of allowing one unusable declaration to invalidate every query.

## Validation

A forced build with `-Wall -Wcompat -Wunused-imports` was warning-clean after
the import cleanup. The normal package checks also passed:

```text
cabal test all --test-show-details=direct   # 33 deterministic regressions
cabal run exference -- --first "a -> a"     # prints id
cabal check                                 # no errors or warnings
cabal sdist
git diff --check
```
