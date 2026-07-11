# Djinn library API hardening (R-10)

- Date: 2026-07-10
- Resolves: finding R-10 of
  [2026-07-10-code-review.md](2026-07-10-code-review.md)
  ("API and display invariants remain public"), in preparation for
  publishing `djinn-core` as a real library
- Toolchain: GHC 9.12.4, Cabal 3.16.1.0, Windows 11

## The problem

`Formula(..)`, `Term(..)`, `HType(..)`, and `HKind(..)` were exported with
raw constructors from flat-named modules, so any library consumer could
hand the machinery negative arities, bad injection indices, duplicate
constructor descriptions, dangling `KVar`s, or cyclic synonym
environments — and some `Show` forms (singleton tuples, empty unions) did
not round-trip through the parser. Fine for an executable-internal AST,
disqualifying for a published API. The flat module names (`HTypes`,
`Environment`, `REPL`, ...) were also unpublishable: they would collide
with other packages in any real build.

## What changed

### 1. The module tree is now a namespace

Everything moved under the package's namespace:

```text
Djinn                      -- the CLI frontend (unchanged behavior)
Djinn.Core                 -- NEW: the stable, validated API
Djinn.Internal.Environment -- formerly Environment
Djinn.Internal.HCheck      -- formerly HCheck        (likewise HIdentifier,
Djinn.Internal.HTypes      --   HTypes, LJT, LJTFormula, ProofCheck,
Djinn.Internal.LJT         --   ProofEnv, REPL, Help)
```

The `Internal` name is the contract: those modules expose raw constructors
and carry no stability or invariant guarantees, which the cabal file and
haddocks now say explicitly.

### 2. `Djinn.Core`: invalid data is unconstructible

The façade covers the full workflow — declarations in, Haskell clauses
out — and validates every boundary:

- `Environment` is **opaque** (read-only accessors only). Values exist
  only via `emptyEnvironment`, `standardEnvironment`, `declare`, and
  `removeDeclaration`, so every stored declaration is lexically named,
  kind-checked, acyclic, and transactionally revalidated; class parameter
  kinds can never go stale.
- `Declaration` is a plain algebraic input type (`TypeSynonym`,
  `DataType`, `AbstractType`, `ClassDecl`, `Function`), so users never
  touch `HTUnion`/`HTAbstract` directly. Names are validated against the
  shared lexical rules (`isConId`, `isVarId`, qualified names for
  functions); `AbstractType` rejects kinds containing `KVar`.
- `HType` and `HKind` are exported **abstractly**, with `parseHType` /
  `parseHKind` (whole-input parsers) and the `kStar`/`kArrow` builders.
  Power users can still import the internal constructors — deliberately,
  and on their own recognizance.
- `inhabit` runs the entire validated pipeline (kind check, context
  resolution against inferred class kinds, budgeted proof search,
  independent proof checking, scope-safe rendering, ranking,
  de-duplication) and reports honestly:

  ```haskell
  data QueryOutcome
      = Realized [String]                 -- best candidate first
      | Unrealizable                      -- decision, not a timeout
      | UnrealizableWithoutSelfReference
      | Undecided                         -- budget expired
  ```

  plus the translated formula and first proof term (as text) for
  debugging. `Left` is reserved for invalid input or internal failures,
  never for "no inhabitant".

### 3. The CLI is now the façade's first consumer

`Djinn.query` and every declaration command delegate to `Djinn.Core`
(`declare`, `removeDeclaration`, `resolveContext`,
`resolveInstanceMethods`, `inhabit`); the shared
shape checks (`requireDistinct`, `checkConstructors`, `checkMethodNames`,
...) moved to `Djinn.Internal.Environment`. There is exactly one
implementation of the pipeline, exercised by all ten CLI scenarios, so
the façade is proven sufficient rather than aspirational. `Djinn.hs`
shrank by roughly 180 lines while total library functionality grew.

Two deliberate user-visible refinements came along: `:delete nosuch` now
says "nosuch is not defined" (the message is produced by the library),
and `:environment` lists the initial classes in declaration order.

### 4. Display invariants documented

The `Show HType` non-round-trip cases (singleton tuples, empty unions
outside declarations) are documented at the instance; `Djinn.Core` cannot
produce such values, and the parser cannot either, so within the façade
`show`/`parseHType` round-trips. The QuickCheck round-trip property
continues to guard the parseable fragment.

## What is deliberately not wrapped

`Term`, `Formula`, and the prover/checker entry points remain
internal-only surface. A future "prove this formula" public API would need
its own input validator (`checkProof` already validates candidate proofs,
but nothing validates caller-supplied formulas); today's façade sidesteps
the issue because every formula it builds comes from a validated `HType`.
Noted as the natural next increment if formula-level access is ever
wanted.

## Validation

- New unit group "validate every boundary of the Djinn.Core facade":
  lowercase/duplicate/recursive/clash/unsolved-kind/ill-kinded rejections,
  transactional removal (undefined, depended-upon, effective), whole-input
  parsing, and honest outcomes (`swap` realized with the canonical clause,
  Peirce decided unrealizable, zero-budget undecided, same-named
  assumption flagged, `Eq` context realized and ranked first,
  kind-mismatched context rejected).
- All suites green (28 unit / 4 properties / 9 CLI); warning-clean build;
  `cabal check` clean; `cabal haddock djinn-core` builds; `cabal sdist`
  packages the relocated tree.
- Full interactive smoke battery after the rewiring: theorem queries,
  data declarations, instances, kind enforcement, budget expiry,
  self-reference diagnosis, deletion paths, and `:environment` — all
  outputs identical up to the two refinements noted above.
