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
  point. It also re-exports the stable Djinn, Exference, and synthesis APIs so a
  consumer can depend on one library while the unified session API develops.
- `synthesis/` supplies the public named `synthesis` sublibrary: validated
  names, types, kinds, declarations, environments, diagnostics, generated
  output, and operational search status.
- `djinn/` supplies the public `djinn-core` proof-search sublibrary and the
  `djinn-frontend` compatibility/REPL sublibrary.
- `exference/` supplies the public parser-independent `exference-core`
  sublibrary and the `exference-frontend` Haskell-source/environment-loader
  sublibrary.

The historical `djinn` and `exference` executable names remain available. The
package also retains `djex-tests`, `synthesis-tests`, all three Djinn test suites,
`exference-tests`, `exference-cli-tests`, and the `djinn-bench` benchmark. This
preserves differential testing between the mature engines until their common
session and frontend can replace the two compatibility surfaces.

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
