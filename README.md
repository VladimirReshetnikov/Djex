# Djex

Djex is the codename for the synthesis tool being formed by merging Djinn and
Exference. The name contracts **Dj**inn and Exference's **ex**. This directory
keeps both independently testable engines beside the neutral foundation that
is progressively replacing their duplicated frontend, validation, environment,
search-envelope, and generated-output infrastructure.

## Components

- `synthesis/` contains the backend-neutral `haskell-synthesis` library.
- `djinn/` contains the intuitionistic LJT proof-search backend and its
  compatibility CLI.
- `exference/` contains the heuristic polymorphic expression-search backend,
  Haskell frontend, environment loader, and compatibility CLI.

The current package names and public modules remain stable while code moves
through checked adapters into `haskell-synthesis`. This preserves differential
testing between the mature engines until their shared session and frontend can
replace the two compatibility surfaces.

## Building

The repository-root Cabal project is the canonical Djex build plan:

```text
cabal build all
cabal test all
```

Each component also retains a local `cabal.project`; from either backend, that
project includes the sibling `../synthesis` package.
